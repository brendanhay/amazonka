{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainContact
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the contact information for a particular domain.
-- Information for at least one contact (registrant, administrator, or
-- technical) must be supplied for update.
--
-- If the update is successful, this method returns an operation ID that
-- you can use to track the progress and completion of the action. If the
-- request is not completed successfully, the domain registrant will be
-- notified by email.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateDomainContact.html>
module Network.AWS.Route53Domains.UpdateDomainContact
    (
    -- * Request
      UpdateDomainContact
    -- ** Request constructor
    , updateDomainContact
    -- ** Request lenses
    , udcRegistrantContact
    , udcAdminContact
    , udcTechContact
    , udcDomainName

    -- * Response
    , UpdateDomainContactResponse
    -- ** Response constructor
    , updateDomainContactResponse
    -- ** Response lenses
    , udcrsStatus
    , udcrsOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The UpdateDomainContact request includes the following elements.
--
-- /See:/ 'updateDomainContact' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcRegistrantContact'
--
-- * 'udcAdminContact'
--
-- * 'udcTechContact'
--
-- * 'udcDomainName'
data UpdateDomainContact = UpdateDomainContact'
    { _udcRegistrantContact :: !(Maybe (Sensitive ContactDetail))
    , _udcAdminContact      :: !(Maybe (Sensitive ContactDetail))
    , _udcTechContact       :: !(Maybe (Sensitive ContactDetail))
    , _udcDomainName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDomainContact' smart constructor.
updateDomainContact :: Text -> UpdateDomainContact
updateDomainContact pDomainName_ =
    UpdateDomainContact'
    { _udcRegistrantContact = Nothing
    , _udcAdminContact = Nothing
    , _udcTechContact = Nothing
    , _udcDomainName = pDomainName_
    }

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
udcRegistrantContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcRegistrantContact = lens _udcRegistrantContact (\ s a -> s{_udcRegistrantContact = a}) . mapping _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
udcAdminContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcAdminContact = lens _udcAdminContact (\ s a -> s{_udcAdminContact = a}) . mapping _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
udcTechContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcTechContact = lens _udcTechContact (\ s a -> s{_udcTechContact = a}) . mapping _Sensitive;

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Internationalized Domain Names
-- are not supported.
--
-- Required: Yes
udcDomainName :: Lens' UpdateDomainContact Text
udcDomainName = lens _udcDomainName (\ s a -> s{_udcDomainName = a});

instance AWSRequest UpdateDomainContact where
        type Sv UpdateDomainContact = Route53Domains
        type Rs UpdateDomainContact =
             UpdateDomainContactResponse
        request = postJSON "UpdateDomainContact"
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDomainContactResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance ToHeaders UpdateDomainContact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.UpdateDomainContact" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDomainContact where
        toJSON UpdateDomainContact'{..}
          = object
              ["RegistrantContact" .= _udcRegistrantContact,
               "AdminContact" .= _udcAdminContact,
               "TechContact" .= _udcTechContact,
               "DomainName" .= _udcDomainName]

instance ToPath UpdateDomainContact where
        toPath = const "/"

instance ToQuery UpdateDomainContact where
        toQuery = const mempty

-- | The UpdateDomainContact response includes the following element.
--
-- /See:/ 'updateDomainContactResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udcrsStatus'
--
-- * 'udcrsOperationId'
data UpdateDomainContactResponse = UpdateDomainContactResponse'
    { _udcrsStatus      :: !Int
    , _udcrsOperationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDomainContactResponse' smart constructor.
updateDomainContactResponse :: Int -> Text -> UpdateDomainContactResponse
updateDomainContactResponse pStatus_ pOperationId_ =
    UpdateDomainContactResponse'
    { _udcrsStatus = pStatus_
    , _udcrsOperationId = pOperationId_
    }

-- | FIXME: Undocumented member.
udcrsStatus :: Lens' UpdateDomainContactResponse Int
udcrsStatus = lens _udcrsStatus (\ s a -> s{_udcrsStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
udcrsOperationId :: Lens' UpdateDomainContactResponse Text
udcrsOperationId = lens _udcrsOperationId (\ s a -> s{_udcrsOperationId = a});
