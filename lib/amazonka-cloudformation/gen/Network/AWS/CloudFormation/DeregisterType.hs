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
-- Module      : Network.AWS.CloudFormation.DeregisterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a type or type version from active use in the CloudFormation registry. If a type or type version is deregistered, it cannot be used in CloudFormation operations.
--
--
-- To deregister a type, you must individually deregister all registered versions of that type. If a type has only a single registered version, deregistering that version results in the type itself being deregistered.
--
-- You cannot deregister the default version of a type, unless it is the only registered version of that type, in which case the type itself is deregistered as well.
module Network.AWS.CloudFormation.DeregisterType
  ( -- * Creating a Request
    deregisterType,
    DeregisterType,

    -- * Request Lenses
    dVersionId,
    dTypeName,
    dARN,
    dType,

    -- * Destructuring the Response
    deregisterTypeResponse,
    DeregisterTypeResponse,

    -- * Response Lenses
    dtrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterType' smart constructor.
data DeregisterType = DeregisterType'
  { _dVersionId :: !(Maybe Text),
    _dTypeName :: !(Maybe Text),
    _dARN :: !(Maybe Text),
    _dType :: !(Maybe RegistryType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVersionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- * 'dTypeName' - The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'dARN' - The Amazon Resource Name (ARN) of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'dType' - The kind of type. Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
deregisterType ::
  DeregisterType
deregisterType =
  DeregisterType'
    { _dVersionId = Nothing,
      _dTypeName = Nothing,
      _dARN = Nothing,
      _dType = Nothing
    }

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
dVersionId :: Lens' DeregisterType (Maybe Text)
dVersionId = lens _dVersionId (\s a -> s {_dVersionId = a})

-- | The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dTypeName :: Lens' DeregisterType (Maybe Text)
dTypeName = lens _dTypeName (\s a -> s {_dTypeName = a})

-- | The Amazon Resource Name (ARN) of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dARN :: Lens' DeregisterType (Maybe Text)
dARN = lens _dARN (\s a -> s {_dARN = a})

-- | The kind of type. Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dType :: Lens' DeregisterType (Maybe RegistryType)
dType = lens _dType (\s a -> s {_dType = a})

instance AWSRequest DeregisterType where
  type Rs DeregisterType = DeregisterTypeResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "DeregisterTypeResult"
      (\s h x -> DeregisterTypeResponse' <$> (pure (fromEnum s)))

instance Hashable DeregisterType

instance NFData DeregisterType

instance ToHeaders DeregisterType where
  toHeaders = const mempty

instance ToPath DeregisterType where
  toPath = const "/"

instance ToQuery DeregisterType where
  toQuery DeregisterType' {..} =
    mconcat
      [ "Action" =: ("DeregisterType" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "VersionId" =: _dVersionId,
        "TypeName" =: _dTypeName,
        "Arn" =: _dARN,
        "Type" =: _dType
      ]

-- | /See:/ 'deregisterTypeResponse' smart constructor.
newtype DeregisterTypeResponse = DeregisterTypeResponse'
  { _dtrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsResponseStatus' - -- | The response status code.
deregisterTypeResponse ::
  -- | 'dtrsResponseStatus'
  Int ->
  DeregisterTypeResponse
deregisterTypeResponse pResponseStatus_ =
  DeregisterTypeResponse' {_dtrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DeregisterTypeResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\s a -> s {_dtrsResponseStatus = a})

instance NFData DeregisterTypeResponse
