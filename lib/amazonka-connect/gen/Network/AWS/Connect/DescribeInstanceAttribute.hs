{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified instance attribute.
module Network.AWS.Connect.DescribeInstanceAttribute
  ( -- * Creating a Request
    describeInstanceAttribute,
    DescribeInstanceAttribute,

    -- * Request Lenses
    diaInstanceId,
    diaAttributeType,

    -- * Destructuring the Response
    describeInstanceAttributeResponse,
    DescribeInstanceAttributeResponse,

    -- * Response Lenses
    diarsAttribute,
    diarsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { _diaInstanceId ::
      !Text,
    _diaAttributeType ::
      !InstanceAttributeType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstanceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diaInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'diaAttributeType' - The type of attribute.
describeInstanceAttribute ::
  -- | 'diaInstanceId'
  Text ->
  -- | 'diaAttributeType'
  InstanceAttributeType ->
  DescribeInstanceAttribute
describeInstanceAttribute pInstanceId_ pAttributeType_ =
  DescribeInstanceAttribute'
    { _diaInstanceId = pInstanceId_,
      _diaAttributeType = pAttributeType_
    }

-- | The identifier of the Amazon Connect instance.
diaInstanceId :: Lens' DescribeInstanceAttribute Text
diaInstanceId = lens _diaInstanceId (\s a -> s {_diaInstanceId = a})

-- | The type of attribute.
diaAttributeType :: Lens' DescribeInstanceAttribute InstanceAttributeType
diaAttributeType = lens _diaAttributeType (\s a -> s {_diaAttributeType = a})

instance AWSRequest DescribeInstanceAttribute where
  type
    Rs DescribeInstanceAttribute =
      DescribeInstanceAttributeResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          DescribeInstanceAttributeResponse'
            <$> (x .?> "Attribute") <*> (pure (fromEnum s))
      )

instance Hashable DescribeInstanceAttribute

instance NFData DescribeInstanceAttribute

instance ToHeaders DescribeInstanceAttribute where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeInstanceAttribute where
  toPath DescribeInstanceAttribute' {..} =
    mconcat
      [ "/instance/",
        toBS _diaInstanceId,
        "/attribute/",
        toBS _diaAttributeType
      ]

instance ToQuery DescribeInstanceAttribute where
  toQuery = const mempty

-- | /See:/ 'describeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { _diarsAttribute ::
      !(Maybe Attribute),
    _diarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstanceAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diarsAttribute' - The type of attribute.
--
-- * 'diarsResponseStatus' - -- | The response status code.
describeInstanceAttributeResponse ::
  -- | 'diarsResponseStatus'
  Int ->
  DescribeInstanceAttributeResponse
describeInstanceAttributeResponse pResponseStatus_ =
  DescribeInstanceAttributeResponse'
    { _diarsAttribute = Nothing,
      _diarsResponseStatus = pResponseStatus_
    }

-- | The type of attribute.
diarsAttribute :: Lens' DescribeInstanceAttributeResponse (Maybe Attribute)
diarsAttribute = lens _diarsAttribute (\s a -> s {_diarsAttribute = a})

-- | -- | The response status code.
diarsResponseStatus :: Lens' DescribeInstanceAttributeResponse Int
diarsResponseStatus = lens _diarsResponseStatus (\s a -> s {_diarsResponseStatus = a})

instance NFData DescribeInstanceAttributeResponse
