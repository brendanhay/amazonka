{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified version from the specified application.
module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
    (
    -- * Request
      DeleteApplicationVersionMessage
    -- ** Request constructor
    , deleteApplicationVersionMessage
    -- ** Request lenses
    , davmApplicationName
    , davmDeleteSourceBundle
    , davmVersionLabel

    -- * Response
    , DeleteApplicationVersionResponse
    -- ** Response constructor
    , deleteApplicationVersionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DeleteApplicationVersionMessage = DeleteApplicationVersionMessage
    { _davmApplicationName    :: Text
    , _davmDeleteSourceBundle :: Maybe Bool
    , _davmVersionLabel       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteApplicationVersionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'davmApplicationName' @::@ 'Text'
--
-- * 'davmDeleteSourceBundle' @::@ 'Maybe' 'Bool'
--
-- * 'davmVersionLabel' @::@ 'Text'
--
deleteApplicationVersionMessage :: Text -- ^ 'davmApplicationName'
                                -> Text -- ^ 'davmVersionLabel'
                                -> DeleteApplicationVersionMessage
deleteApplicationVersionMessage p1 p2 = DeleteApplicationVersionMessage
    { _davmApplicationName    = p1
    , _davmVersionLabel       = p2
    , _davmDeleteSourceBundle = Nothing
    }

-- | The name of the application to delete releases from.
davmApplicationName :: Lens' DeleteApplicationVersionMessage Text
davmApplicationName =
    lens _davmApplicationName (\s a -> s { _davmApplicationName = a })

-- | Indicates whether to delete the associated source bundle from Amazon S3:
-- true: An attempt is made to delete the associated Amazon S3 source bundle
-- specified at time of creation. false: No action is taken on the Amazon S3
-- source bundle specified at time of creation. Valid Values: true | false.
davmDeleteSourceBundle :: Lens' DeleteApplicationVersionMessage (Maybe Bool)
davmDeleteSourceBundle =
    lens _davmDeleteSourceBundle (\s a -> s { _davmDeleteSourceBundle = a })

-- | The label of the version to delete.
davmVersionLabel :: Lens' DeleteApplicationVersionMessage Text
davmVersionLabel = lens _davmVersionLabel (\s a -> s { _davmVersionLabel = a })

instance ToQuery DeleteApplicationVersionMessage

instance ToPath DeleteApplicationVersionMessage where
    toPath = const "/"

data DeleteApplicationVersionResponse = DeleteApplicationVersionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteApplicationVersionResponse' constructor.
deleteApplicationVersionResponse :: DeleteApplicationVersionResponse
deleteApplicationVersionResponse = DeleteApplicationVersionResponse

instance FromXML DeleteApplicationVersionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteApplicationVersionResponse"

instance AWSRequest DeleteApplicationVersionMessage where
    type Sv DeleteApplicationVersionMessage = ElasticBeanstalk
    type Rs DeleteApplicationVersionMessage = DeleteApplicationVersionResponse

    request  = post "DeleteApplicationVersion"
    response = nullaryResponse DeleteApplicationVersionResponse
