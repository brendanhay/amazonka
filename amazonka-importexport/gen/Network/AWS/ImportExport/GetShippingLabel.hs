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
-- Module      : Network.AWS.ImportExport.GetShippingLabel
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation generates a pre-paid UPS shipping label that you will use
-- to ship your device to AWS for processing.
--
-- /See:/ <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebGetShippingLabel.html AWS API Reference> for GetShippingLabel.
module Network.AWS.ImportExport.GetShippingLabel
    (
    -- * Creating a Request
      getShippingLabel
    , GetShippingLabel
    -- * Request Lenses
    , gslStreet3
    , gslAPIVersion
    , gslPostalCode
    , gslCountry
    , gslStateOrProvince
    , gslStreet2
    , gslName
    , gslCompany
    , gslCity
    , gslPhoneNumber
    , gslStreet1
    , gslJobIds

    -- * Destructuring the Response
    , getShippingLabelResponse
    , GetShippingLabelResponse
    -- * Response Lenses
    , gslrsShippingLabelURL
    , gslrsWarning
    , gslrsStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.ImportExport.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getShippingLabel' smart constructor.
data GetShippingLabel = GetShippingLabel'
    { _gslStreet3         :: !(Maybe Text)
    , _gslAPIVersion      :: !(Maybe Text)
    , _gslPostalCode      :: !(Maybe Text)
    , _gslCountry         :: !(Maybe Text)
    , _gslStateOrProvince :: !(Maybe Text)
    , _gslStreet2         :: !(Maybe Text)
    , _gslName            :: !(Maybe Text)
    , _gslCompany         :: !(Maybe Text)
    , _gslCity            :: !(Maybe Text)
    , _gslPhoneNumber     :: !(Maybe Text)
    , _gslStreet1         :: !(Maybe Text)
    , _gslJobIds          :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetShippingLabel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gslStreet3'
--
-- * 'gslAPIVersion'
--
-- * 'gslPostalCode'
--
-- * 'gslCountry'
--
-- * 'gslStateOrProvince'
--
-- * 'gslStreet2'
--
-- * 'gslName'
--
-- * 'gslCompany'
--
-- * 'gslCity'
--
-- * 'gslPhoneNumber'
--
-- * 'gslStreet1'
--
-- * 'gslJobIds'
getShippingLabel
    :: GetShippingLabel
getShippingLabel =
    GetShippingLabel'
    { _gslStreet3 = Nothing
    , _gslAPIVersion = Nothing
    , _gslPostalCode = Nothing
    , _gslCountry = Nothing
    , _gslStateOrProvince = Nothing
    , _gslStreet2 = Nothing
    , _gslName = Nothing
    , _gslCompany = Nothing
    , _gslCity = Nothing
    , _gslPhoneNumber = Nothing
    , _gslStreet1 = Nothing
    , _gslJobIds = mempty
    }

-- | Undocumented member.
gslStreet3 :: Lens' GetShippingLabel (Maybe Text)
gslStreet3 = lens _gslStreet3 (\ s a -> s{_gslStreet3 = a});

-- | Undocumented member.
gslAPIVersion :: Lens' GetShippingLabel (Maybe Text)
gslAPIVersion = lens _gslAPIVersion (\ s a -> s{_gslAPIVersion = a});

-- | Undocumented member.
gslPostalCode :: Lens' GetShippingLabel (Maybe Text)
gslPostalCode = lens _gslPostalCode (\ s a -> s{_gslPostalCode = a});

-- | Undocumented member.
gslCountry :: Lens' GetShippingLabel (Maybe Text)
gslCountry = lens _gslCountry (\ s a -> s{_gslCountry = a});

-- | Undocumented member.
gslStateOrProvince :: Lens' GetShippingLabel (Maybe Text)
gslStateOrProvince = lens _gslStateOrProvince (\ s a -> s{_gslStateOrProvince = a});

-- | Undocumented member.
gslStreet2 :: Lens' GetShippingLabel (Maybe Text)
gslStreet2 = lens _gslStreet2 (\ s a -> s{_gslStreet2 = a});

-- | Undocumented member.
gslName :: Lens' GetShippingLabel (Maybe Text)
gslName = lens _gslName (\ s a -> s{_gslName = a});

-- | Undocumented member.
gslCompany :: Lens' GetShippingLabel (Maybe Text)
gslCompany = lens _gslCompany (\ s a -> s{_gslCompany = a});

-- | Undocumented member.
gslCity :: Lens' GetShippingLabel (Maybe Text)
gslCity = lens _gslCity (\ s a -> s{_gslCity = a});

-- | Undocumented member.
gslPhoneNumber :: Lens' GetShippingLabel (Maybe Text)
gslPhoneNumber = lens _gslPhoneNumber (\ s a -> s{_gslPhoneNumber = a});

-- | Undocumented member.
gslStreet1 :: Lens' GetShippingLabel (Maybe Text)
gslStreet1 = lens _gslStreet1 (\ s a -> s{_gslStreet1 = a});

-- | Undocumented member.
gslJobIds :: Lens' GetShippingLabel [Text]
gslJobIds = lens _gslJobIds (\ s a -> s{_gslJobIds = a}) . _Coerce;

instance AWSRequest GetShippingLabel where
        type Rs GetShippingLabel = GetShippingLabelResponse
        request = postQuery importExport
        response
          = receiveXMLWrapper "GetShippingLabelResult"
              (\ s h x ->
                 GetShippingLabelResponse' <$>
                   (x .@? "ShippingLabelURL") <*> (x .@? "Warning") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetShippingLabel where
        toHeaders = const mempty

instance ToPath GetShippingLabel where
        toPath = const "/"

instance ToQuery GetShippingLabel where
        toQuery GetShippingLabel'{..}
          = mconcat
              ["Operation=GetShippingLabel",
               "Action" =: ("GetShippingLabel" :: ByteString),
               "Version" =: ("2010-06-01" :: ByteString),
               "street3" =: _gslStreet3,
               "APIVersion" =: _gslAPIVersion,
               "postalCode" =: _gslPostalCode,
               "country" =: _gslCountry,
               "stateOrProvince" =: _gslStateOrProvince,
               "street2" =: _gslStreet2, "name" =: _gslName,
               "company" =: _gslCompany, "city" =: _gslCity,
               "phoneNumber" =: _gslPhoneNumber,
               "street1" =: _gslStreet1,
               "jobIds" =: toQueryList "member" _gslJobIds]

-- | /See:/ 'getShippingLabelResponse' smart constructor.
data GetShippingLabelResponse = GetShippingLabelResponse'
    { _gslrsShippingLabelURL :: !(Maybe Text)
    , _gslrsWarning          :: !(Maybe Text)
    , _gslrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetShippingLabelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gslrsShippingLabelURL'
--
-- * 'gslrsWarning'
--
-- * 'gslrsStatus'
getShippingLabelResponse
    :: Int -- ^ 'gslrsStatus'
    -> GetShippingLabelResponse
getShippingLabelResponse pStatus_ =
    GetShippingLabelResponse'
    { _gslrsShippingLabelURL = Nothing
    , _gslrsWarning = Nothing
    , _gslrsStatus = pStatus_
    }

-- | Undocumented member.
gslrsShippingLabelURL :: Lens' GetShippingLabelResponse (Maybe Text)
gslrsShippingLabelURL = lens _gslrsShippingLabelURL (\ s a -> s{_gslrsShippingLabelURL = a});

-- | Undocumented member.
gslrsWarning :: Lens' GetShippingLabelResponse (Maybe Text)
gslrsWarning = lens _gslrsWarning (\ s a -> s{_gslrsWarning = a});

-- | The response status code.
gslrsStatus :: Lens' GetShippingLabelResponse Int
gslrsStatus = lens _gslrsStatus (\ s a -> s{_gslrsStatus = a});
