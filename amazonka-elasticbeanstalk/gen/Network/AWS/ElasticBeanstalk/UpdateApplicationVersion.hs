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
    , uavDescription
    , uavApplicationName
    , uavVersionLabel

    -- * Response
    , ApplicationVersionDescriptionMessage
    -- ** Response constructor
    , applicationVersionDescriptionMessage
    -- ** Response lenses
    , avdmApplicationVersion
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
-- * 'uavDescription'
--
-- * 'uavApplicationName'
--
-- * 'uavVersionLabel'
data UpdateApplicationVersion = UpdateApplicationVersion'
    { _uavDescription     :: !(Maybe Text)
    , _uavApplicationName :: !Text
    , _uavVersionLabel    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateApplicationVersion' smart constructor.
updateApplicationVersion :: Text -> Text -> UpdateApplicationVersion
updateApplicationVersion pApplicationName_ pVersionLabel_ =
    UpdateApplicationVersion'
    { _uavDescription = Nothing
    , _uavApplicationName = pApplicationName_
    , _uavVersionLabel = pVersionLabel_
    }

-- | A new description for this release.
uavDescription :: Lens' UpdateApplicationVersion (Maybe Text)
uavDescription = lens _uavDescription (\ s a -> s{_uavDescription = a});

-- | The name of the application associated with this version.
--
-- If no application is found with this name, @UpdateApplication@ returns
-- an @InvalidParameterValue@ error.
uavApplicationName :: Lens' UpdateApplicationVersion Text
uavApplicationName = lens _uavApplicationName (\ s a -> s{_uavApplicationName = a});

-- | The name of the version to update.
--
-- If no application version is found with this label, @UpdateApplication@
-- returns an @InvalidParameterValue@ error.
uavVersionLabel :: Lens' UpdateApplicationVersion Text
uavVersionLabel = lens _uavVersionLabel (\ s a -> s{_uavVersionLabel = a});

instance AWSRequest UpdateApplicationVersion where
        type Sv UpdateApplicationVersion = ElasticBeanstalk
        type Rs UpdateApplicationVersion =
             ApplicationVersionDescriptionMessage
        request = post "UpdateApplicationVersion"
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
               "Description" =: _uavDescription,
               "ApplicationName" =: _uavApplicationName,
               "VersionLabel" =: _uavVersionLabel]
