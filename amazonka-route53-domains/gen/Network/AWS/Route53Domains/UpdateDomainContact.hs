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
-- Module      : Network.AWS.Route53Domains.UpdateDomainContact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the contact information for a particular domain. You must specify information for at least one contact: registrant, administrator, or technical.
--
--
-- If the update is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
--
module Network.AWS.Route53Domains.UpdateDomainContact
    (
    -- * Creating a Request
      updateDomainContact
    , UpdateDomainContact
    -- * Request Lenses
    , udcRegistrantContact
    , udcAdminContact
    , udcTechContact
    , udcDomainName

    -- * Destructuring the Response
    , updateDomainContactResponse
    , UpdateDomainContactResponse
    -- * Response Lenses
    , udcrsResponseStatus
    , udcrsOperationId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The UpdateDomainContact request includes the following elements.
--
--
--
-- /See:/ 'updateDomainContact' smart constructor.
data UpdateDomainContact = UpdateDomainContact'
  { _udcRegistrantContact :: !(Maybe (Sensitive ContactDetail))
  , _udcAdminContact      :: !(Maybe (Sensitive ContactDetail))
  , _udcTechContact       :: !(Maybe (Sensitive ContactDetail))
  , _udcDomainName        :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcRegistrantContact' - Provides detailed contact information.
--
-- * 'udcAdminContact' - Provides detailed contact information.
--
-- * 'udcTechContact' - Provides detailed contact information.
--
-- * 'udcDomainName' - The name of the domain that you want to update contact information for.
updateDomainContact
    :: Text -- ^ 'udcDomainName'
    -> UpdateDomainContact
updateDomainContact pDomainName_ =
  UpdateDomainContact'
    { _udcRegistrantContact = Nothing
    , _udcAdminContact = Nothing
    , _udcTechContact = Nothing
    , _udcDomainName = pDomainName_
    }


-- | Provides detailed contact information.
udcRegistrantContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcRegistrantContact = lens _udcRegistrantContact (\ s a -> s{_udcRegistrantContact = a}) . mapping _Sensitive

-- | Provides detailed contact information.
udcAdminContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcAdminContact = lens _udcAdminContact (\ s a -> s{_udcAdminContact = a}) . mapping _Sensitive

-- | Provides detailed contact information.
udcTechContact :: Lens' UpdateDomainContact (Maybe ContactDetail)
udcTechContact = lens _udcTechContact (\ s a -> s{_udcTechContact = a}) . mapping _Sensitive

-- | The name of the domain that you want to update contact information for.
udcDomainName :: Lens' UpdateDomainContact Text
udcDomainName = lens _udcDomainName (\ s a -> s{_udcDomainName = a})

instance AWSRequest UpdateDomainContact where
        type Rs UpdateDomainContact =
             UpdateDomainContactResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDomainContactResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance Hashable UpdateDomainContact where

instance NFData UpdateDomainContact where

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
              (catMaybes
                 [("RegistrantContact" .=) <$> _udcRegistrantContact,
                  ("AdminContact" .=) <$> _udcAdminContact,
                  ("TechContact" .=) <$> _udcTechContact,
                  Just ("DomainName" .= _udcDomainName)])

instance ToPath UpdateDomainContact where
        toPath = const "/"

instance ToQuery UpdateDomainContact where
        toQuery = const mempty

-- | The UpdateDomainContact response includes the following element.
--
--
--
-- /See:/ 'updateDomainContactResponse' smart constructor.
data UpdateDomainContactResponse = UpdateDomainContactResponse'
  { _udcrsResponseStatus :: !Int
  , _udcrsOperationId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcrsResponseStatus' - -- | The response status code.
--
-- * 'udcrsOperationId' - Identifier for tracking the progress of the request. To use this ID to query the operation status, use 'GetOperationDetail' .
updateDomainContactResponse
    :: Int -- ^ 'udcrsResponseStatus'
    -> Text -- ^ 'udcrsOperationId'
    -> UpdateDomainContactResponse
updateDomainContactResponse pResponseStatus_ pOperationId_ =
  UpdateDomainContactResponse'
    {_udcrsResponseStatus = pResponseStatus_, _udcrsOperationId = pOperationId_}


-- | -- | The response status code.
udcrsResponseStatus :: Lens' UpdateDomainContactResponse Int
udcrsResponseStatus = lens _udcrsResponseStatus (\ s a -> s{_udcrsResponseStatus = a})

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use 'GetOperationDetail' .
udcrsOperationId :: Lens' UpdateDomainContactResponse Text
udcrsOperationId = lens _udcrsOperationId (\ s a -> s{_udcrsOperationId = a})

instance NFData UpdateDomainContactResponse where
