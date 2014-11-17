{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.UpdateDomainContact
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the contact information for a particular domain.
-- Information for at least one contact (registrant, administrator, or
-- technical) must be supplied for update. If the update is successful, this
-- method returns an operation ID that you can use to track the progress and
-- completion of the action. If the request is not completed successfully, the
-- domain registrant will be notified by email.
--
-- <UpdateDomainContact.html>
module Network.AWS.Route53Domains.UpdateDomainContact
    (
    -- * Request
      UpdateDomainContact
    -- ** Request constructor
    , updateDomainContact
    -- ** Request lenses
    , udcAdminContact
    , udcDomainName
    , udcRegistrantContact
    , udcTechContact

    -- * Response
    , UpdateDomainContactResponse
    -- ** Response constructor
    , updateDomainContactResponse
    -- ** Response lenses
    , udcrOperationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

data UpdateDomainContact = UpdateDomainContact
    { _udcAdminContact      :: Maybe ContactDetail
    , _udcDomainName        :: Text
    , _udcRegistrantContact :: Maybe ContactDetail
    , _udcTechContact       :: Maybe ContactDetail
    } deriving (Eq, Show, Generic)

-- | 'UpdateDomainContact' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcAdminContact' @::@ 'Maybe' 'ContactDetail'
--
-- * 'udcDomainName' @::@ 'Text'
--
-- * 'udcRegistrantContact' @::@ 'Maybe' 'ContactDetail'
--
-- * 'udcTechContact' @::@ 'Maybe' 'ContactDetail'
--
updateDomainContact :: Text -- ^ 'udcDomainName'
                    -> UpdateDomainContact
updateDomainContact p1 = UpdateDomainContact
    { _udcDomainName        = p1
    , _udcAdminContact      = Nothing
    , _udcRegistrantContact = Nothing
    , _udcTechContact       = Nothing
    }

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
udcAdminContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcAdminContact = lens _udcAdminContact (\s a -> s { _udcAdminContact = a })

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9,
-- and hyphen (-). Internationalized Domain Names are not supported.
-- Required: Yes.
udcDomainName :: Lens' UpdateDomainContact Text
udcDomainName = lens _udcDomainName (\s a -> s { _udcDomainName = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
udcRegistrantContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcRegistrantContact =
    lens _udcRegistrantContact (\s a -> s { _udcRegistrantContact = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
udcTechContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcTechContact = lens _udcTechContact (\s a -> s { _udcTechContact = a })

newtype UpdateDomainContactResponse = UpdateDomainContactResponse
    { _udcrOperationId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'UpdateDomainContactResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcrOperationId' @::@ 'Text'
--
updateDomainContactResponse :: Text -- ^ 'udcrOperationId'
                            -> UpdateDomainContactResponse
updateDomainContactResponse p1 = UpdateDomainContactResponse
    { _udcrOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
udcrOperationId :: Lens' UpdateDomainContactResponse Text
udcrOperationId = lens _udcrOperationId (\s a -> s { _udcrOperationId = a })

instance AWSRequest UpdateDomainContact where
    type Sv UpdateDomainContact = Route53Domains
    type Rs UpdateDomainContact = UpdateDomainContactResponse

    request  = post
    response = jsonResponse

instance FromJSON UpdateDomainContactResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath UpdateDomainContact where
    toPath = const "/"

instance ToHeaders UpdateDomainContact

instance ToQuery UpdateDomainContact where
    toQuery = const mempty

instance ToJSON UpdateDomainContact where
    toJSON = genericToJSON jsonOptions
