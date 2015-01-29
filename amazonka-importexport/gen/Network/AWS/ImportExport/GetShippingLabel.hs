{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.GetShippingLabel
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns information about a job, including where the job is in
-- the processing pipeline, the status of the results, and the signature value
-- associated with the job. You can only return information about jobs you own.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebGetShippingLabel.html>
module Network.AWS.ImportExport.GetShippingLabel
    (
    -- * Request
      GetShippingLabel
    -- ** Request constructor
    , getShippingLabel
    -- ** Request lenses
    , gslAPIVersion
    , gslCity
    , gslCompany
    , gslCountry
    , gslJobIds
    , gslName
    , gslPhoneNumber
    , gslPostalCode
    , gslStateOrProvince
    , gslStreet1
    , gslStreet2
    , gslStreet3

    -- * Response
    , GetShippingLabelResponse
    -- ** Response constructor
    , getShippingLabelResponse
    -- ** Response lenses
    , gslrShippingLabelURL
    , gslrWarning
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types
import qualified GHC.Exts

data GetShippingLabel = GetShippingLabel
    { _gslAPIVersion      :: Maybe Text
    , _gslCity            :: Maybe Text
    , _gslCompany         :: Maybe Text
    , _gslCountry         :: Maybe Text
    , _gslJobIds          :: List "member" Text
    , _gslName            :: Maybe Text
    , _gslPhoneNumber     :: Maybe Text
    , _gslPostalCode      :: Maybe Text
    , _gslStateOrProvince :: Maybe Text
    , _gslStreet1         :: Maybe Text
    , _gslStreet2         :: Maybe Text
    , _gslStreet3         :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GetShippingLabel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gslAPIVersion' @::@ 'Maybe' 'Text'
--
-- * 'gslCity' @::@ 'Maybe' 'Text'
--
-- * 'gslCompany' @::@ 'Maybe' 'Text'
--
-- * 'gslCountry' @::@ 'Maybe' 'Text'
--
-- * 'gslJobIds' @::@ ['Text']
--
-- * 'gslName' @::@ 'Maybe' 'Text'
--
-- * 'gslPhoneNumber' @::@ 'Maybe' 'Text'
--
-- * 'gslPostalCode' @::@ 'Maybe' 'Text'
--
-- * 'gslStateOrProvince' @::@ 'Maybe' 'Text'
--
-- * 'gslStreet1' @::@ 'Maybe' 'Text'
--
-- * 'gslStreet2' @::@ 'Maybe' 'Text'
--
-- * 'gslStreet3' @::@ 'Maybe' 'Text'
--
getShippingLabel :: GetShippingLabel
getShippingLabel = GetShippingLabel
    { _gslJobIds          = mempty
    , _gslName            = Nothing
    , _gslCompany         = Nothing
    , _gslPhoneNumber     = Nothing
    , _gslCountry         = Nothing
    , _gslStateOrProvince = Nothing
    , _gslCity            = Nothing
    , _gslPostalCode      = Nothing
    , _gslStreet1         = Nothing
    , _gslStreet2         = Nothing
    , _gslStreet3         = Nothing
    , _gslAPIVersion      = Nothing
    }

gslAPIVersion :: Lens' GetShippingLabel (Maybe Text)
gslAPIVersion = lens _gslAPIVersion (\s a -> s { _gslAPIVersion = a })

gslCity :: Lens' GetShippingLabel (Maybe Text)
gslCity = lens _gslCity (\s a -> s { _gslCity = a })

gslCompany :: Lens' GetShippingLabel (Maybe Text)
gslCompany = lens _gslCompany (\s a -> s { _gslCompany = a })

gslCountry :: Lens' GetShippingLabel (Maybe Text)
gslCountry = lens _gslCountry (\s a -> s { _gslCountry = a })

gslJobIds :: Lens' GetShippingLabel [Text]
gslJobIds = lens _gslJobIds (\s a -> s { _gslJobIds = a }) . _List

gslName :: Lens' GetShippingLabel (Maybe Text)
gslName = lens _gslName (\s a -> s { _gslName = a })

gslPhoneNumber :: Lens' GetShippingLabel (Maybe Text)
gslPhoneNumber = lens _gslPhoneNumber (\s a -> s { _gslPhoneNumber = a })

gslPostalCode :: Lens' GetShippingLabel (Maybe Text)
gslPostalCode = lens _gslPostalCode (\s a -> s { _gslPostalCode = a })

gslStateOrProvince :: Lens' GetShippingLabel (Maybe Text)
gslStateOrProvince =
    lens _gslStateOrProvince (\s a -> s { _gslStateOrProvince = a })

gslStreet1 :: Lens' GetShippingLabel (Maybe Text)
gslStreet1 = lens _gslStreet1 (\s a -> s { _gslStreet1 = a })

gslStreet2 :: Lens' GetShippingLabel (Maybe Text)
gslStreet2 = lens _gslStreet2 (\s a -> s { _gslStreet2 = a })

gslStreet3 :: Lens' GetShippingLabel (Maybe Text)
gslStreet3 = lens _gslStreet3 (\s a -> s { _gslStreet3 = a })

data GetShippingLabelResponse = GetShippingLabelResponse
    { _gslrShippingLabelURL :: Maybe Text
    , _gslrWarning          :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GetShippingLabelResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gslrShippingLabelURL' @::@ 'Maybe' 'Text'
--
-- * 'gslrWarning' @::@ 'Maybe' 'Text'
--
getShippingLabelResponse :: GetShippingLabelResponse
getShippingLabelResponse = GetShippingLabelResponse
    { _gslrShippingLabelURL = Nothing
    , _gslrWarning          = Nothing
    }

gslrShippingLabelURL :: Lens' GetShippingLabelResponse (Maybe Text)
gslrShippingLabelURL =
    lens _gslrShippingLabelURL (\s a -> s { _gslrShippingLabelURL = a })

gslrWarning :: Lens' GetShippingLabelResponse (Maybe Text)
gslrWarning = lens _gslrWarning (\s a -> s { _gslrWarning = a })

instance ToPath GetShippingLabel where
    toPath = const "/"

instance ToQuery GetShippingLabel where
    toQuery GetShippingLabel{..} = mconcat
        [ "APIVersion"      =? _gslAPIVersion
        , "city"            =? _gslCity
        , "company"         =? _gslCompany
        , "country"         =? _gslCountry
        , "jobIds"          =? _gslJobIds
        , "name"            =? _gslName
        , "phoneNumber"     =? _gslPhoneNumber
        , "postalCode"      =? _gslPostalCode
        , "stateOrProvince" =? _gslStateOrProvince
        , "street1"         =? _gslStreet1
        , "street2"         =? _gslStreet2
        , "street3"         =? _gslStreet3
        ]

instance ToHeaders GetShippingLabel

instance AWSRequest GetShippingLabel where
    type Sv GetShippingLabel = ImportExport
    type Rs GetShippingLabel = GetShippingLabelResponse

    request  = post "GetShippingLabel"
    response = xmlResponse

instance FromXML GetShippingLabelResponse where
    parseXML = withElement "GetShippingLabelResult" $ \x -> GetShippingLabelResponse
        <$> x .@? "ShippingLabelURL"
        <*> x .@? "Warning"
