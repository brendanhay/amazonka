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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application version to have the specified properties.
--
--
module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
    (
    -- * Creating a Request
      updateApplicationVersion
    , UpdateApplicationVersion
    -- * Request Lenses
    , uavDescription
    , uavApplicationName
    , uavVersionLabel

    -- * Destructuring the Response
    , applicationVersionDescriptionMessage
    , ApplicationVersionDescriptionMessage
    -- * Response Lenses
    , avdmApplicationVersion
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'updateApplicationVersion' smart constructor.
data UpdateApplicationVersion = UpdateApplicationVersion'
  { _uavDescription     :: !(Maybe Text)
  , _uavApplicationName :: !Text
  , _uavVersionLabel    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uavDescription' - A new description for this version.
--
-- * 'uavApplicationName' - The name of the application associated with this version. If no application is found with this name, @UpdateApplication@ returns an @InvalidParameterValue@ error.
--
-- * 'uavVersionLabel' - The name of the version to update. If no application version is found with this label, @UpdateApplication@ returns an @InvalidParameterValue@ error.
updateApplicationVersion
    :: Text -- ^ 'uavApplicationName'
    -> Text -- ^ 'uavVersionLabel'
    -> UpdateApplicationVersion
updateApplicationVersion pApplicationName_ pVersionLabel_ =
  UpdateApplicationVersion'
    { _uavDescription = Nothing
    , _uavApplicationName = pApplicationName_
    , _uavVersionLabel = pVersionLabel_
    }


-- | A new description for this version.
uavDescription :: Lens' UpdateApplicationVersion (Maybe Text)
uavDescription = lens _uavDescription (\ s a -> s{_uavDescription = a})

-- | The name of the application associated with this version. If no application is found with this name, @UpdateApplication@ returns an @InvalidParameterValue@ error.
uavApplicationName :: Lens' UpdateApplicationVersion Text
uavApplicationName = lens _uavApplicationName (\ s a -> s{_uavApplicationName = a})

-- | The name of the version to update. If no application version is found with this label, @UpdateApplication@ returns an @InvalidParameterValue@ error.
uavVersionLabel :: Lens' UpdateApplicationVersion Text
uavVersionLabel = lens _uavVersionLabel (\ s a -> s{_uavVersionLabel = a})

instance AWSRequest UpdateApplicationVersion where
        type Rs UpdateApplicationVersion =
             ApplicationVersionDescriptionMessage
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "UpdateApplicationVersionResult"
              (\ s h x -> parseXML x)

instance Hashable UpdateApplicationVersion where

instance NFData UpdateApplicationVersion where

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
