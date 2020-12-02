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
-- Module      : Network.AWS.ServiceCatalog.DescribeServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a self-service action.
module Network.AWS.ServiceCatalog.DescribeServiceAction
  ( -- * Creating a Request
    describeServiceAction,
    DescribeServiceAction,

    -- * Request Lenses
    dsaAcceptLanguage,
    dsaId,

    -- * Destructuring the Response
    describeServiceActionResponse,
    DescribeServiceActionResponse,

    -- * Response Lenses
    dsarsServiceActionDetail,
    dsarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'describeServiceAction' smart constructor.
data DescribeServiceAction = DescribeServiceAction'
  { _dsaAcceptLanguage ::
      !(Maybe Text),
    _dsaId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeServiceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dsaId' - The self-service action identifier.
describeServiceAction ::
  -- | 'dsaId'
  Text ->
  DescribeServiceAction
describeServiceAction pId_ =
  DescribeServiceAction'
    { _dsaAcceptLanguage = Nothing,
      _dsaId = pId_
    }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dsaAcceptLanguage :: Lens' DescribeServiceAction (Maybe Text)
dsaAcceptLanguage = lens _dsaAcceptLanguage (\s a -> s {_dsaAcceptLanguage = a})

-- | The self-service action identifier.
dsaId :: Lens' DescribeServiceAction Text
dsaId = lens _dsaId (\s a -> s {_dsaId = a})

instance AWSRequest DescribeServiceAction where
  type Rs DescribeServiceAction = DescribeServiceActionResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          DescribeServiceActionResponse'
            <$> (x .?> "ServiceActionDetail") <*> (pure (fromEnum s))
      )

instance Hashable DescribeServiceAction

instance NFData DescribeServiceAction

instance ToHeaders DescribeServiceAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.DescribeServiceAction" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeServiceAction where
  toJSON DescribeServiceAction' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _dsaAcceptLanguage,
            Just ("Id" .= _dsaId)
          ]
      )

instance ToPath DescribeServiceAction where
  toPath = const "/"

instance ToQuery DescribeServiceAction where
  toQuery = const mempty

-- | /See:/ 'describeServiceActionResponse' smart constructor.
data DescribeServiceActionResponse = DescribeServiceActionResponse'
  { _dsarsServiceActionDetail ::
      !(Maybe ServiceActionDetail),
    _dsarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeServiceActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarsServiceActionDetail' - Detailed information about the self-service action.
--
-- * 'dsarsResponseStatus' - -- | The response status code.
describeServiceActionResponse ::
  -- | 'dsarsResponseStatus'
  Int ->
  DescribeServiceActionResponse
describeServiceActionResponse pResponseStatus_ =
  DescribeServiceActionResponse'
    { _dsarsServiceActionDetail =
        Nothing,
      _dsarsResponseStatus = pResponseStatus_
    }

-- | Detailed information about the self-service action.
dsarsServiceActionDetail :: Lens' DescribeServiceActionResponse (Maybe ServiceActionDetail)
dsarsServiceActionDetail = lens _dsarsServiceActionDetail (\s a -> s {_dsarsServiceActionDetail = a})

-- | -- | The response status code.
dsarsResponseStatus :: Lens' DescribeServiceActionResponse Int
dsarsResponseStatus = lens _dsarsResponseStatus (\s a -> s {_dsarsResponseStatus = a})

instance NFData DescribeServiceActionResponse
