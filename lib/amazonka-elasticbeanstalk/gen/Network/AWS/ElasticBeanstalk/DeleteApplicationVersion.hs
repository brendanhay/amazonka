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
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified application.
--
--
module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
    (
    -- * Creating a Request
      deleteApplicationVersion
    , DeleteApplicationVersion
    -- * Request Lenses
    , davDeleteSourceBundle
    , davApplicationName
    , davVersionLabel

    -- * Destructuring the Response
    , deleteApplicationVersionResponse
    , DeleteApplicationVersionResponse
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to delete an application version.
--
--
--
-- /See:/ 'deleteApplicationVersion' smart constructor.
data DeleteApplicationVersion = DeleteApplicationVersion'
  { _davDeleteSourceBundle :: !(Maybe Bool)
  , _davApplicationName    :: !Text
  , _davVersionLabel       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davDeleteSourceBundle' - Set to @true@ to delete the source bundle from your storage bucket. Otherwise, the application version is deleted only from Elastic Beanstalk and the source bundle remains in Amazon S3.
--
-- * 'davApplicationName' - The name of the application to which the version belongs.
--
-- * 'davVersionLabel' - The label of the version to delete.
deleteApplicationVersion
    :: Text -- ^ 'davApplicationName'
    -> Text -- ^ 'davVersionLabel'
    -> DeleteApplicationVersion
deleteApplicationVersion pApplicationName_ pVersionLabel_ =
  DeleteApplicationVersion'
    { _davDeleteSourceBundle = Nothing
    , _davApplicationName = pApplicationName_
    , _davVersionLabel = pVersionLabel_
    }


-- | Set to @true@ to delete the source bundle from your storage bucket. Otherwise, the application version is deleted only from Elastic Beanstalk and the source bundle remains in Amazon S3.
davDeleteSourceBundle :: Lens' DeleteApplicationVersion (Maybe Bool)
davDeleteSourceBundle = lens _davDeleteSourceBundle (\ s a -> s{_davDeleteSourceBundle = a})

-- | The name of the application to which the version belongs.
davApplicationName :: Lens' DeleteApplicationVersion Text
davApplicationName = lens _davApplicationName (\ s a -> s{_davApplicationName = a})

-- | The label of the version to delete.
davVersionLabel :: Lens' DeleteApplicationVersion Text
davVersionLabel = lens _davVersionLabel (\ s a -> s{_davVersionLabel = a})

instance AWSRequest DeleteApplicationVersion where
        type Rs DeleteApplicationVersion =
             DeleteApplicationVersionResponse
        request = postQuery elasticBeanstalk
        response
          = receiveNull DeleteApplicationVersionResponse'

instance Hashable DeleteApplicationVersion where

instance NFData DeleteApplicationVersion where

instance ToHeaders DeleteApplicationVersion where
        toHeaders = const mempty

instance ToPath DeleteApplicationVersion where
        toPath = const "/"

instance ToQuery DeleteApplicationVersion where
        toQuery DeleteApplicationVersion'{..}
          = mconcat
              ["Action" =:
                 ("DeleteApplicationVersion" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "DeleteSourceBundle" =: _davDeleteSourceBundle,
               "ApplicationName" =: _davApplicationName,
               "VersionLabel" =: _davVersionLabel]

-- | /See:/ 'deleteApplicationVersionResponse' smart constructor.
data DeleteApplicationVersionResponse =
  DeleteApplicationVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationVersionResponse' with the minimum fields required to make a request.
--
deleteApplicationVersionResponse
    :: DeleteApplicationVersionResponse
deleteApplicationVersionResponse = DeleteApplicationVersionResponse'


instance NFData DeleteApplicationVersionResponse
         where
