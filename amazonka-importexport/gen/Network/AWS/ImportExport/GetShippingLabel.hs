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
    , gslrqStreet3
    , gslrqAPIVersion
    , gslrqPostalCode
    , gslrqCountry
    , gslrqStateOrProvince
    , gslrqStreet2
    , gslrqName
    , gslrqCompany
    , gslrqCity
    , gslrqPhoneNumber
    , gslrqStreet1
    , gslrqJobIds

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
-- * 'gslrqStreet3'
--
-- * 'gslrqAPIVersion'
--
-- * 'gslrqPostalCode'
--
-- * 'gslrqCountry'
--
-- * 'gslrqStateOrProvince'
--
-- * 'gslrqStreet2'
--
-- * 'gslrqName'
--
-- * 'gslrqCompany'
--
-- * 'gslrqCity'
--
-- * 'gslrqPhoneNumber'
--
-- * 'gslrqStreet1'
--
-- * 'gslrqJobIds'
data GetShippingLabel = GetShippingLabel'
    { _gslrqStreet3         :: !(Maybe Text)
    , _gslrqAPIVersion      :: !(Maybe Text)
    , _gslrqPostalCode      :: !(Maybe Text)
    , _gslrqCountry         :: !(Maybe Text)
    , _gslrqStateOrProvince :: !(Maybe Text)
    , _gslrqStreet2         :: !(Maybe Text)
    , _gslrqName            :: !(Maybe Text)
    , _gslrqCompany         :: !(Maybe Text)
    , _gslrqCity            :: !(Maybe Text)
    , _gslrqPhoneNumber     :: !(Maybe Text)
    , _gslrqStreet1         :: !(Maybe Text)
    , _gslrqJobIds          :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetShippingLabel' smart constructor.
getShippingLabel :: GetShippingLabel
getShippingLabel =
    GetShippingLabel'
    { _gslrqStreet3 = Nothing
    , _gslrqAPIVersion = Nothing
    , _gslrqPostalCode = Nothing
    , _gslrqCountry = Nothing
    , _gslrqStateOrProvince = Nothing
    , _gslrqStreet2 = Nothing
    , _gslrqName = Nothing
    , _gslrqCompany = Nothing
    , _gslrqCity = Nothing
    , _gslrqPhoneNumber = Nothing
    , _gslrqStreet1 = Nothing
    , _gslrqJobIds = mempty
    }

-- | FIXME: Undocumented member.
gslrqStreet3 :: Lens' GetShippingLabel (Maybe Text)
gslrqStreet3 = lens _gslrqStreet3 (\ s a -> s{_gslrqStreet3 = a});

-- | FIXME: Undocumented member.
gslrqAPIVersion :: Lens' GetShippingLabel (Maybe Text)
gslrqAPIVersion = lens _gslrqAPIVersion (\ s a -> s{_gslrqAPIVersion = a});

-- | FIXME: Undocumented member.
gslrqPostalCode :: Lens' GetShippingLabel (Maybe Text)
gslrqPostalCode = lens _gslrqPostalCode (\ s a -> s{_gslrqPostalCode = a});

-- | FIXME: Undocumented member.
gslrqCountry :: Lens' GetShippingLabel (Maybe Text)
gslrqCountry = lens _gslrqCountry (\ s a -> s{_gslrqCountry = a});

-- | FIXME: Undocumented member.
gslrqStateOrProvince :: Lens' GetShippingLabel (Maybe Text)
gslrqStateOrProvince = lens _gslrqStateOrProvince (\ s a -> s{_gslrqStateOrProvince = a});

-- | FIXME: Undocumented member.
gslrqStreet2 :: Lens' GetShippingLabel (Maybe Text)
gslrqStreet2 = lens _gslrqStreet2 (\ s a -> s{_gslrqStreet2 = a});

-- | FIXME: Undocumented member.
gslrqName :: Lens' GetShippingLabel (Maybe Text)
gslrqName = lens _gslrqName (\ s a -> s{_gslrqName = a});

-- | FIXME: Undocumented member.
gslrqCompany :: Lens' GetShippingLabel (Maybe Text)
gslrqCompany = lens _gslrqCompany (\ s a -> s{_gslrqCompany = a});

-- | FIXME: Undocumented member.
gslrqCity :: Lens' GetShippingLabel (Maybe Text)
gslrqCity = lens _gslrqCity (\ s a -> s{_gslrqCity = a});

-- | FIXME: Undocumented member.
gslrqPhoneNumber :: Lens' GetShippingLabel (Maybe Text)
gslrqPhoneNumber = lens _gslrqPhoneNumber (\ s a -> s{_gslrqPhoneNumber = a});

-- | FIXME: Undocumented member.
gslrqStreet1 :: Lens' GetShippingLabel (Maybe Text)
gslrqStreet1 = lens _gslrqStreet1 (\ s a -> s{_gslrqStreet1 = a});

-- | FIXME: Undocumented member.
gslrqJobIds :: Lens' GetShippingLabel [Text]
gslrqJobIds = lens _gslrqJobIds (\ s a -> s{_gslrqJobIds = a});

instance AWSRequest GetShippingLabel where
        type Sv GetShippingLabel = ImportExport
        type Rs GetShippingLabel = GetShippingLabelResponse
        request = post
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
               "street3" =: _gslrqStreet3,
               "APIVersion" =: _gslrqAPIVersion,
               "postalCode" =: _gslrqPostalCode,
               "country" =: _gslrqCountry,
               "stateOrProvince" =: _gslrqStateOrProvince,
               "street2" =: _gslrqStreet2, "name" =: _gslrqName,
               "company" =: _gslrqCompany, "city" =: _gslrqCity,
               "phoneNumber" =: _gslrqPhoneNumber,
               "street1" =: _gslrqStreet1,
               "jobIds" =: toQueryList "member" _gslrqJobIds]

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
getShippingLabelResponse pStatus =
    GetShippingLabelResponse'
    { _gslrsShippingLabelURL = Nothing
    , _gslrsWarning = Nothing
    , _gslrsStatus = pStatus
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
