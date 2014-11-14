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

-- Module      : Network.AWS.CodeDeploy.RegisterApplicationRevision
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers with AWS CodeDeploy a revision for the specified application.
module Network.AWS.CodeDeploy.RegisterApplicationRevision
    (
    -- * Request
      RegisterApplicationRevision
    -- ** Request constructor
    , registerApplicationRevision
    -- ** Request lenses
    , rarApplicationName
    , rarDescription
    , rarRevision

    -- * Response
    , RegisterApplicationRevisionResponse
    -- ** Response constructor
    , registerApplicationRevisionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CodeDeploy.Types

data RegisterApplicationRevision = RegisterApplicationRevision
    { _rarApplicationName :: Text
    , _rarDescription     :: Maybe Text
    , _rarRevision        :: RevisionLocation
    } deriving (Eq, Show, Generic)

-- | 'RegisterApplicationRevision' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rarApplicationName' @::@ 'Text'
--
-- * 'rarDescription' @::@ 'Maybe' 'Text'
--
-- * 'rarRevision' @::@ 'RevisionLocation'
--
registerApplicationRevision :: Text -- ^ 'rarApplicationName'
                            -> RevisionLocation -- ^ 'rarRevision'
                            -> RegisterApplicationRevision
registerApplicationRevision p1 p2 = RegisterApplicationRevision
    { _rarApplicationName = p1
    , _rarRevision        = p2
    , _rarDescription     = Nothing
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
rarApplicationName :: Lens' RegisterApplicationRevision Text
rarApplicationName =
    lens _rarApplicationName (\s a -> s { _rarApplicationName = a })

-- | A comment about the revision.
rarDescription :: Lens' RegisterApplicationRevision (Maybe Text)
rarDescription = lens _rarDescription (\s a -> s { _rarDescription = a })

-- | Information about the application revision to register, including the
-- revision's type and its location.
rarRevision :: Lens' RegisterApplicationRevision RevisionLocation
rarRevision = lens _rarRevision (\s a -> s { _rarRevision = a })

instance ToPath RegisterApplicationRevision where
    toPath = const "/"

instance ToQuery RegisterApplicationRevision where
    toQuery = const mempty

instance ToHeaders RegisterApplicationRevision

instance ToBody RegisterApplicationRevision where
    toBody = toBody . encode . _rarApplicationName

data RegisterApplicationRevisionResponse = RegisterApplicationRevisionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RegisterApplicationRevisionResponse' constructor.
registerApplicationRevisionResponse :: RegisterApplicationRevisionResponse
registerApplicationRevisionResponse = RegisterApplicationRevisionResponse

instance AWSRequest RegisterApplicationRevision where
    type Sv RegisterApplicationRevision = CodeDeploy
    type Rs RegisterApplicationRevision = RegisterApplicationRevisionResponse

    request  = post
    response = nullaryResponse RegisterApplicationRevisionResponse
