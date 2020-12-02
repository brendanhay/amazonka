{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeleteOrganization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the organization. You can delete an organization only by using credentials from the master account. The organization must be empty of member accounts, OUs, and policies.
--
--
module Network.AWS.Organizations.DeleteOrganization
    (
    -- * Creating a Request
      deleteOrganization
    , DeleteOrganization

    -- * Destructuring the Response
    , deleteOrganizationResponse
    , DeleteOrganizationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOrganization' smart constructor.
data DeleteOrganization =
  DeleteOrganization'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOrganization' with the minimum fields required to make a request.
--
deleteOrganization
    :: DeleteOrganization
deleteOrganization = DeleteOrganization'


instance AWSRequest DeleteOrganization where
        type Rs DeleteOrganization =
             DeleteOrganizationResponse
        request = postJSON organizations
        response = receiveNull DeleteOrganizationResponse'

instance Hashable DeleteOrganization where

instance NFData DeleteOrganization where

instance ToHeaders DeleteOrganization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DeleteOrganization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteOrganization where
        toJSON = const (Object mempty)

instance ToPath DeleteOrganization where
        toPath = const "/"

instance ToQuery DeleteOrganization where
        toQuery = const mempty

-- | /See:/ 'deleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse =
  DeleteOrganizationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOrganizationResponse' with the minimum fields required to make a request.
--
deleteOrganizationResponse
    :: DeleteOrganizationResponse
deleteOrganizationResponse = DeleteOrganizationResponse'


instance NFData DeleteOrganizationResponse where
