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
-- Module      : Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the Access Status for AWS Organization portfolio share feature. This API can only be called by the management account in the organization or by a delegated admin.
module Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
  ( -- * Creating a Request
    getAWSOrganizationsAccessStatus,
    GetAWSOrganizationsAccessStatus,

    -- * Destructuring the Response
    getAWSOrganizationsAccessStatusResponse,
    GetAWSOrganizationsAccessStatusResponse,

    -- * Response Lenses
    gaoasrsAccessStatus,
    gaoasrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'getAWSOrganizationsAccessStatus' smart constructor.
data GetAWSOrganizationsAccessStatus = GetAWSOrganizationsAccessStatus'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAWSOrganizationsAccessStatus' with the minimum fields required to make a request.
getAWSOrganizationsAccessStatus ::
  GetAWSOrganizationsAccessStatus
getAWSOrganizationsAccessStatus = GetAWSOrganizationsAccessStatus'

instance AWSRequest GetAWSOrganizationsAccessStatus where
  type
    Rs GetAWSOrganizationsAccessStatus =
      GetAWSOrganizationsAccessStatusResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          GetAWSOrganizationsAccessStatusResponse'
            <$> (x .?> "AccessStatus") <*> (pure (fromEnum s))
      )

instance Hashable GetAWSOrganizationsAccessStatus

instance NFData GetAWSOrganizationsAccessStatus

instance ToHeaders GetAWSOrganizationsAccessStatus where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.GetAWSOrganizationsAccessStatus" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetAWSOrganizationsAccessStatus where
  toJSON = const (Object mempty)

instance ToPath GetAWSOrganizationsAccessStatus where
  toPath = const "/"

instance ToQuery GetAWSOrganizationsAccessStatus where
  toQuery = const mempty

-- | /See:/ 'getAWSOrganizationsAccessStatusResponse' smart constructor.
data GetAWSOrganizationsAccessStatusResponse = GetAWSOrganizationsAccessStatusResponse'
  { _gaoasrsAccessStatus ::
      !( Maybe
           AccessStatus
       ),
    _gaoasrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAWSOrganizationsAccessStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaoasrsAccessStatus' - The status of the portfolio share feature.
--
-- * 'gaoasrsResponseStatus' - -- | The response status code.
getAWSOrganizationsAccessStatusResponse ::
  -- | 'gaoasrsResponseStatus'
  Int ->
  GetAWSOrganizationsAccessStatusResponse
getAWSOrganizationsAccessStatusResponse pResponseStatus_ =
  GetAWSOrganizationsAccessStatusResponse'
    { _gaoasrsAccessStatus =
        Nothing,
      _gaoasrsResponseStatus = pResponseStatus_
    }

-- | The status of the portfolio share feature.
gaoasrsAccessStatus :: Lens' GetAWSOrganizationsAccessStatusResponse (Maybe AccessStatus)
gaoasrsAccessStatus = lens _gaoasrsAccessStatus (\s a -> s {_gaoasrsAccessStatus = a})

-- | -- | The response status code.
gaoasrsResponseStatus :: Lens' GetAWSOrganizationsAccessStatusResponse Int
gaoasrsResponseStatus = lens _gaoasrsResponseStatus (\s a -> s {_gaoasrsResponseStatus = a})

instance NFData GetAWSOrganizationsAccessStatusResponse
