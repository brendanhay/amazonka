{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application version to have the specified
-- properties.
--
-- If a property (for example, @description@) is not provided, the value
-- remains unchanged. To clear properties, specify an empty string.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateApplicationVersion.html>
module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
    (
    -- * Request
      UpdateApplicationVersion
    -- ** Request constructor
    , updateApplicationVersion
    -- ** Request lenses
    , uavrqDescription
    , uavrqApplicationName
    , uavrqVersionLabel

    -- * Response
    , ApplicationVersionDescriptionMessage
    -- ** Response constructor
    , applicationVersionDescriptionMessage
    -- ** Response lenses
    , uavrsApplicationVersion
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'updateApplicationVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uavrqDescription'
--
-- * 'uavrqApplicationName'
--
-- * 'uavrqVersionLabel'
data UpdateApplicationVersion = UpdateApplicationVersion'
    { _uavrqDescription     :: !(Maybe Text)
    , _uavrqApplicationName :: !Text
    , _uavrqVersionLabel    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateApplicationVersion' smart constructor.
updateApplicationVersion :: Text -> Text -> UpdateApplicationVersion
updateApplicationVersion pApplicationName pVersionLabel =
    UpdateApplicationVersion'
    { _uavrqDescription = Nothing
    , _uavrqApplicationName = pApplicationName
    , _uavrqVersionLabel = pVersionLabel
    }

-- | A new description for this release.
uavrqDescription :: Lens' UpdateApplicationVersion (Maybe Text)
uavrqDescription = lens _uavrqDescription (\ s a -> s{_uavrqDescription = a});

-- | The name of the application associated with this version.
--
-- If no application is found with this name, @UpdateApplication@ returns
-- an @InvalidParameterValue@ error.
uavrqApplicationName :: Lens' UpdateApplicationVersion Text
uavrqApplicationName = lens _uavrqApplicationName (\ s a -> s{_uavrqApplicationName = a});

-- | The name of the version to update.
--
-- If no application version is found with this label, @UpdateApplication@
-- returns an @InvalidParameterValue@ error.
uavrqVersionLabel :: Lens' UpdateApplicationVersion Text
uavrqVersionLabel = lens _uavrqVersionLabel (\ s a -> s{_uavrqVersionLabel = a});

instance AWSRequest UpdateApplicationVersion where
        type Sv UpdateApplicationVersion = ElasticBeanstalk
        type Rs UpdateApplicationVersion =
             ApplicationVersionDescriptionMessage
        request = post
        response
          = receiveXMLWrapper "UpdateApplicationVersionResult"
              (\ s h x -> parseXML x)

instance ToHeaders UpdateApplicationVersion where
        toHeaders = const mempty

instance ToPath UpdateApplicationVersion where
        toPath = const "/"

instance ToQuery UpdateApplicationVersion where
        toQuery UpdateApplicationVersion'{..}
          = mconcat
              ["Action" =:
                 ("UpdateApplicationVersion" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Description" =: _uavrqDescription,
               "ApplicationName" =: _uavrqApplicationName,
               "VersionLabel" =: _uavrqVersionLabel]
