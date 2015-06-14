{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified version from the specified application.
--
-- You cannot delete an application version that is associated with a
-- running environment.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DeleteApplicationVersion.html>
module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
    (
    -- * Request
      DeleteApplicationVersion
    -- ** Request constructor
    , deleteApplicationVersion
    -- ** Request lenses
    , davDeleteSourceBundle
    , davApplicationName
    , davVersionLabel

    -- * Response
    , DeleteApplicationVersionResponse
    -- ** Response constructor
    , deleteApplicationVersionResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'deleteApplicationVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'davDeleteSourceBundle'
--
-- * 'davApplicationName'
--
-- * 'davVersionLabel'
data DeleteApplicationVersion = DeleteApplicationVersion'{_davDeleteSourceBundle :: Maybe Bool, _davApplicationName :: Text, _davVersionLabel :: Text} deriving (Eq, Read, Show)

-- | 'DeleteApplicationVersion' smart constructor.
deleteApplicationVersion :: Text -> Text -> DeleteApplicationVersion
deleteApplicationVersion pApplicationName pVersionLabel = DeleteApplicationVersion'{_davDeleteSourceBundle = Nothing, _davApplicationName = pApplicationName, _davVersionLabel = pVersionLabel};

-- | Indicates whether to delete the associated source bundle from Amazon S3:
--
-- -   @true@: An attempt is made to delete the associated Amazon S3 source
--     bundle specified at time of creation.
-- -   @false@: No action is taken on the Amazon S3 source bundle specified
--     at time of creation.
--
-- Valid Values: @true@ | @false@
davDeleteSourceBundle :: Lens' DeleteApplicationVersion (Maybe Bool)
davDeleteSourceBundle = lens _davDeleteSourceBundle (\ s a -> s{_davDeleteSourceBundle = a});

-- | The name of the application to delete releases from.
davApplicationName :: Lens' DeleteApplicationVersion Text
davApplicationName = lens _davApplicationName (\ s a -> s{_davApplicationName = a});

-- | The label of the version to delete.
davVersionLabel :: Lens' DeleteApplicationVersion Text
davVersionLabel = lens _davVersionLabel (\ s a -> s{_davVersionLabel = a});

instance AWSRequest DeleteApplicationVersion where
        type Sv DeleteApplicationVersion = ElasticBeanstalk
        type Rs DeleteApplicationVersion =
             DeleteApplicationVersionResponse
        request = post
        response
          = receiveNull DeleteApplicationVersionResponse'

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
data DeleteApplicationVersionResponse = DeleteApplicationVersionResponse' deriving (Eq, Read, Show)

-- | 'DeleteApplicationVersionResponse' smart constructor.
deleteApplicationVersionResponse :: DeleteApplicationVersionResponse
deleteApplicationVersionResponse = DeleteApplicationVersionResponse';
