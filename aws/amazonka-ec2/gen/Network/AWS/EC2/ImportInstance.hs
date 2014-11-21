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

-- Module      : Network.AWS.EC2.ImportInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an import instance task using metadata from the specified disk
-- image. After importing the image, you then upload it using the
-- ec2-import-volume command in the EC2 command line tools. For more
-- information, see Using the Command Line Tools to Import Your Virtual
-- Machine to Amazon EC2 in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportInstance.html>
module Network.AWS.EC2.ImportInstance
    (
    -- * Request
      ImportInstance
    -- ** Request constructor
    , importInstance
    -- ** Request lenses
    , iiDescription
    , iiDiskImages
    , iiDryRun
    , iiLaunchSpecification
    , iiPlatform

    -- * Response
    , ImportInstanceResponse
    -- ** Response constructor
    , importInstanceResponse
    -- ** Response lenses
    , iirConversionTask
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ImportInstance = ImportInstance
    { _iiDescription         :: Maybe Text
    , _iiDiskImages          :: List "diskImage" DiskImage
    , _iiDryRun              :: Maybe Bool
    , _iiLaunchSpecification :: Maybe ImportInstanceLaunchSpecification
    , _iiPlatform            :: Text
    } deriving (Eq, Show)

-- | 'ImportInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iiDescription' @::@ 'Maybe' 'Text'
--
-- * 'iiDiskImages' @::@ ['DiskImage']
--
-- * 'iiDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'iiLaunchSpecification' @::@ 'Maybe' 'ImportInstanceLaunchSpecification'
--
-- * 'iiPlatform' @::@ 'Text'
--
importInstance :: Text -- ^ 'iiPlatform'
               -> ImportInstance
importInstance p1 = ImportInstance
    { _iiPlatform            = p1
    , _iiDryRun              = Nothing
    , _iiDescription         = Nothing
    , _iiLaunchSpecification = Nothing
    , _iiDiskImages          = mempty
    }

-- | A description for the instance being imported.
iiDescription :: Lens' ImportInstance (Maybe Text)
iiDescription = lens _iiDescription (\s a -> s { _iiDescription = a })

iiDiskImages :: Lens' ImportInstance [DiskImage]
iiDiskImages = lens _iiDiskImages (\s a -> s { _iiDiskImages = a }) . _List

iiDryRun :: Lens' ImportInstance (Maybe Bool)
iiDryRun = lens _iiDryRun (\s a -> s { _iiDryRun = a })

-- | 
iiLaunchSpecification :: Lens' ImportInstance (Maybe ImportInstanceLaunchSpecification)
iiLaunchSpecification =
    lens _iiLaunchSpecification (\s a -> s { _iiLaunchSpecification = a })

-- | The instance operating system.
iiPlatform :: Lens' ImportInstance Text
iiPlatform = lens _iiPlatform (\s a -> s { _iiPlatform = a })

newtype ImportInstanceResponse = ImportInstanceResponse
    { _iirConversionTask :: Maybe ConversionTask
    } deriving (Eq, Show)

-- | 'ImportInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iirConversionTask' @::@ 'Maybe' 'ConversionTask'
--
importInstanceResponse :: ImportInstanceResponse
importInstanceResponse = ImportInstanceResponse
    { _iirConversionTask = Nothing
    }

iirConversionTask :: Lens' ImportInstanceResponse (Maybe ConversionTask)
iirConversionTask =
    lens _iirConversionTask (\s a -> s { _iirConversionTask = a })

instance ToPath ImportInstance where
    toPath = const "/"

instance ToQuery ImportInstance where
    toQuery ImportInstance{..} = mconcat
        [ "description"         =? _iiDescription
        , "diskImage"           =? _iiDiskImages
        , "dryRun"              =? _iiDryRun
        , "launchSpecification" =? _iiLaunchSpecification
        , "platform"            =? _iiPlatform
        ]

instance ToHeaders ImportInstance

instance AWSRequest ImportInstance where
    type Sv ImportInstance = EC2
    type Rs ImportInstance = ImportInstanceResponse

    request  = post "ImportInstance"
    response = xmlResponse

instance FromXML ImportInstanceResponse where
    parseXML x = ImportInstanceResponse
        <$> x .@? "conversionTask"
