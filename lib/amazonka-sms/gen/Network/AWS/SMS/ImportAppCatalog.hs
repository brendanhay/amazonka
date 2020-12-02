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
-- Module      : Network.AWS.SMS.ImportAppCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows application import from AWS Migration Hub.
module Network.AWS.SMS.ImportAppCatalog
  ( -- * Creating a Request
    importAppCatalog,
    ImportAppCatalog,

    -- * Request Lenses
    iacRoleName,

    -- * Destructuring the Response
    importAppCatalogResponse,
    ImportAppCatalogResponse,

    -- * Response Lenses
    iacrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'importAppCatalog' smart constructor.
newtype ImportAppCatalog = ImportAppCatalog'
  { _iacRoleName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportAppCatalog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iacRoleName' - The name of the service role. If you omit this parameter, we create a service-linked role for AWS Migration Hub in your account. Otherwise, the role that you provide must have the <https://docs.aws.amazon.com/migrationhub/latest/ug/new-customer-setup.html#sms-managed policy and trust policy> described in the /AWS Migration Hub User Guide/ .
importAppCatalog ::
  ImportAppCatalog
importAppCatalog = ImportAppCatalog' {_iacRoleName = Nothing}

-- | The name of the service role. If you omit this parameter, we create a service-linked role for AWS Migration Hub in your account. Otherwise, the role that you provide must have the <https://docs.aws.amazon.com/migrationhub/latest/ug/new-customer-setup.html#sms-managed policy and trust policy> described in the /AWS Migration Hub User Guide/ .
iacRoleName :: Lens' ImportAppCatalog (Maybe Text)
iacRoleName = lens _iacRoleName (\s a -> s {_iacRoleName = a})

instance AWSRequest ImportAppCatalog where
  type Rs ImportAppCatalog = ImportAppCatalogResponse
  request = postJSON sms
  response =
    receiveEmpty
      (\s h x -> ImportAppCatalogResponse' <$> (pure (fromEnum s)))

instance Hashable ImportAppCatalog

instance NFData ImportAppCatalog

instance ToHeaders ImportAppCatalog where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.ImportAppCatalog" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ImportAppCatalog where
  toJSON ImportAppCatalog' {..} =
    object (catMaybes [("roleName" .=) <$> _iacRoleName])

instance ToPath ImportAppCatalog where
  toPath = const "/"

instance ToQuery ImportAppCatalog where
  toQuery = const mempty

-- | /See:/ 'importAppCatalogResponse' smart constructor.
newtype ImportAppCatalogResponse = ImportAppCatalogResponse'
  { _iacrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportAppCatalogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iacrsResponseStatus' - -- | The response status code.
importAppCatalogResponse ::
  -- | 'iacrsResponseStatus'
  Int ->
  ImportAppCatalogResponse
importAppCatalogResponse pResponseStatus_ =
  ImportAppCatalogResponse'
    { _iacrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
iacrsResponseStatus :: Lens' ImportAppCatalogResponse Int
iacrsResponseStatus = lens _iacrsResponseStatus (\s a -> s {_iacrsResponseStatus = a})

instance NFData ImportAppCatalogResponse
