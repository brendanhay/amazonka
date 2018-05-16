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
-- Module      : Network.AWS.Organizations.DeleteOrganizationalUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an organizational unit from a root or another OU. You must first remove all accounts and child OUs from the OU that you want to delete.
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.DeleteOrganizationalUnit
    (
    -- * Creating a Request
      deleteOrganizationalUnit
    , DeleteOrganizationalUnit
    -- * Request Lenses
    , dOrganizationalUnitId

    -- * Destructuring the Response
    , deleteOrganizationalUnitResponse
    , DeleteOrganizationalUnitResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOrganizationalUnit' smart constructor.
newtype DeleteOrganizationalUnit = DeleteOrganizationalUnit'
  { _dOrganizationalUnitId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOrganizationalUnit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dOrganizationalUnitId' - The unique identifier (ID) of the organizational unit that you want to delete. You can get the ID from the 'ListOrganizationalUnitsForParent' operation. The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
deleteOrganizationalUnit
    :: Text -- ^ 'dOrganizationalUnitId'
    -> DeleteOrganizationalUnit
deleteOrganizationalUnit pOrganizationalUnitId_ =
  DeleteOrganizationalUnit' {_dOrganizationalUnitId = pOrganizationalUnitId_}


-- | The unique identifier (ID) of the organizational unit that you want to delete. You can get the ID from the 'ListOrganizationalUnitsForParent' operation. The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
dOrganizationalUnitId :: Lens' DeleteOrganizationalUnit Text
dOrganizationalUnitId = lens _dOrganizationalUnitId (\ s a -> s{_dOrganizationalUnitId = a})

instance AWSRequest DeleteOrganizationalUnit where
        type Rs DeleteOrganizationalUnit =
             DeleteOrganizationalUnitResponse
        request = postJSON organizations
        response
          = receiveNull DeleteOrganizationalUnitResponse'

instance Hashable DeleteOrganizationalUnit where

instance NFData DeleteOrganizationalUnit where

instance ToHeaders DeleteOrganizationalUnit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DeleteOrganizationalUnit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteOrganizationalUnit where
        toJSON DeleteOrganizationalUnit'{..}
          = object
              (catMaybes
                 [Just
                    ("OrganizationalUnitId" .= _dOrganizationalUnitId)])

instance ToPath DeleteOrganizationalUnit where
        toPath = const "/"

instance ToQuery DeleteOrganizationalUnit where
        toQuery = const mempty

-- | /See:/ 'deleteOrganizationalUnitResponse' smart constructor.
data DeleteOrganizationalUnitResponse =
  DeleteOrganizationalUnitResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteOrganizationalUnitResponse' with the minimum fields required to make a request.
--
deleteOrganizationalUnitResponse
    :: DeleteOrganizationalUnitResponse
deleteOrganizationalUnitResponse = DeleteOrganizationalUnitResponse'


instance NFData DeleteOrganizationalUnitResponse
         where
