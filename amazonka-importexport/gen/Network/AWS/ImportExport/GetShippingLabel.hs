{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.GetShippingLabel
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation generates a pre-paid UPS shipping label that you will use
-- to ship your device to AWS for processing.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebGetShippingLabel.html>
module Network.AWS.ImportExport.GetShippingLabel
    (
    -- * Request
      GetShippingLabel
    -- ** Request constructor
    , getShippingLabel
    -- ** Request lenses
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

    -- * Response
    , GetShippingLabelResponse
    -- ** Response constructor
    , getShippingLabelResponse
    -- ** Response lenses
    , gslrsShippingLabelURL
    , gslrsWarning
    , gslrsStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getShippingLabel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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

-- | 'GetShippingLabel' smart constructor.
getShippingLabel :: GetShippingLabel
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

-- | FIXME: Undocumented member.
gslStreet3 :: Lens' GetShippingLabel (Maybe Text)
gslStreet3 = lens _gslStreet3 (\ s a -> s{_gslStreet3 = a});

-- | FIXME: Undocumented member.
gslAPIVersion :: Lens' GetShippingLabel (Maybe Text)
gslAPIVersion = lens _gslAPIVersion (\ s a -> s{_gslAPIVersion = a});

-- | FIXME: Undocumented member.
gslPostalCode :: Lens' GetShippingLabel (Maybe Text)
gslPostalCode = lens _gslPostalCode (\ s a -> s{_gslPostalCode = a});

-- | FIXME: Undocumented member.
gslCountry :: Lens' GetShippingLabel (Maybe Text)
gslCountry = lens _gslCountry (\ s a -> s{_gslCountry = a});

-- | FIXME: Undocumented member.
gslStateOrProvince :: Lens' GetShippingLabel (Maybe Text)
gslStateOrProvince = lens _gslStateOrProvince (\ s a -> s{_gslStateOrProvince = a});

-- | FIXME: Undocumented member.
gslStreet2 :: Lens' GetShippingLabel (Maybe Text)
gslStreet2 = lens _gslStreet2 (\ s a -> s{_gslStreet2 = a});

-- | FIXME: Undocumented member.
gslName :: Lens' GetShippingLabel (Maybe Text)
gslName = lens _gslName (\ s a -> s{_gslName = a});

-- | FIXME: Undocumented member.
gslCompany :: Lens' GetShippingLabel (Maybe Text)
gslCompany = lens _gslCompany (\ s a -> s{_gslCompany = a});

-- | FIXME: Undocumented member.
gslCity :: Lens' GetShippingLabel (Maybe Text)
gslCity = lens _gslCity (\ s a -> s{_gslCity = a});

-- | FIXME: Undocumented member.
gslPhoneNumber :: Lens' GetShippingLabel (Maybe Text)
gslPhoneNumber = lens _gslPhoneNumber (\ s a -> s{_gslPhoneNumber = a});

-- | FIXME: Undocumented member.
gslStreet1 :: Lens' GetShippingLabel (Maybe Text)
gslStreet1 = lens _gslStreet1 (\ s a -> s{_gslStreet1 = a});

-- | FIXME: Undocumented member.
gslJobIds :: Lens' GetShippingLabel [Text]
gslJobIds = lens _gslJobIds (\ s a -> s{_gslJobIds = a}) . _Coerce;

instance AWSRequest GetShippingLabel where
        type Sv GetShippingLabel = ImportExport
        type Rs GetShippingLabel = GetShippingLabelResponse
        request = postQuery
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gslrsShippingLabelURL'
--
-- * 'gslrsWarning'
--
-- * 'gslrsStatus'
data GetShippingLabelResponse = GetShippingLabelResponse'
    { _gslrsShippingLabelURL :: !(Maybe Text)
    , _gslrsWarning          :: !(Maybe Text)
    , _gslrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetShippingLabelResponse' smart constructor.
getShippingLabelResponse :: Int -> GetShippingLabelResponse
getShippingLabelResponse pStatus_ =
    GetShippingLabelResponse'
    { _gslrsShippingLabelURL = Nothing
    , _gslrsWarning = Nothing
    , _gslrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
gslrsShippingLabelURL :: Lens' GetShippingLabelResponse (Maybe Text)
gslrsShippingLabelURL = lens _gslrsShippingLabelURL (\ s a -> s{_gslrsShippingLabelURL = a});

-- | FIXME: Undocumented member.
gslrsWarning :: Lens' GetShippingLabelResponse (Maybe Text)
gslrsWarning = lens _gslrsWarning (\ s a -> s{_gslrsWarning = a});

-- | FIXME: Undocumented member.
gslrsStatus :: Lens' GetShippingLabelResponse Int
gslrsStatus = lens _gslrsStatus (\ s a -> s{_gslrsStatus = a});
