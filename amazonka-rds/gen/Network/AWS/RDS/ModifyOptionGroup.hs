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

-- Module      : Network.AWS.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing option group.
module Network.AWS.RDS.ModifyOptionGroup
    (
    -- * Request
      ModifyOptionGroupMessage
    -- ** Request constructor
    , modifyOptionGroup
    -- ** Request lenses
    , mogmApplyImmediately
    , mogmOptionGroupName
    , mogmOptionsToInclude
    , mogmOptionsToRemove

    -- * Response
    , ModifyOptionGroupResult
    -- ** Response constructor
    , modifyOptionGroupResponse
    -- ** Response lenses
    , mogrOptionGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data ModifyOptionGroupMessage = ModifyOptionGroupMessage
    { _mogmApplyImmediately :: Maybe Bool
    , _mogmOptionGroupName  :: Text
    , _mogmOptionsToInclude :: [OptionConfiguration]
    , _mogmOptionsToRemove  :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'ModifyOptionGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mogmApplyImmediately' @::@ 'Maybe' 'Bool'
--
-- * 'mogmOptionGroupName' @::@ 'Text'
--
-- * 'mogmOptionsToInclude' @::@ ['OptionConfiguration']
--
-- * 'mogmOptionsToRemove' @::@ ['Text']
--
modifyOptionGroup :: Text -- ^ 'mogmOptionGroupName'
                  -> ModifyOptionGroupMessage
modifyOptionGroup p1 = ModifyOptionGroupMessage
    { _mogmOptionGroupName  = p1
    , _mogmOptionsToInclude = mempty
    , _mogmOptionsToRemove  = mempty
    , _mogmApplyImmediately = Nothing
    }

-- | Indicates whether the changes should be applied immediately, or during
-- the next maintenance window for each instance associated with the option
-- group.
mogmApplyImmediately :: Lens' ModifyOptionGroupMessage (Maybe Bool)
mogmApplyImmediately =
    lens _mogmApplyImmediately (\s a -> s { _mogmApplyImmediately = a })

-- | The name of the option group to be modified. Permanent options, such as
-- the TDE option for Oracle Advanced Security TDE, cannot be removed from
-- an option group, and that option group cannot be removed from a DB
-- instance once it is associated with a DB instance.
mogmOptionGroupName :: Lens' ModifyOptionGroupMessage Text
mogmOptionGroupName =
    lens _mogmOptionGroupName (\s a -> s { _mogmOptionGroupName = a })

-- | Options in this list are added to the option group or, if already
-- present, the specified configuration is used to update the existing
-- configuration.
mogmOptionsToInclude :: Lens' ModifyOptionGroupMessage [OptionConfiguration]
mogmOptionsToInclude =
    lens _mogmOptionsToInclude (\s a -> s { _mogmOptionsToInclude = a })

-- | Options in this list are removed from the option group.
mogmOptionsToRemove :: Lens' ModifyOptionGroupMessage [Text]
mogmOptionsToRemove =
    lens _mogmOptionsToRemove (\s a -> s { _mogmOptionsToRemove = a })

instance ToPath ModifyOptionGroupMessage where
    toPath = const "/"

instance ToQuery ModifyOptionGroupMessage

newtype ModifyOptionGroupResult = ModifyOptionGroupResult
    { _mogrOptionGroup :: Maybe OptionGroup
    } deriving (Eq, Show, Generic)

-- | 'ModifyOptionGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mogrOptionGroup' @::@ 'Maybe' 'OptionGroup'
--
modifyOptionGroupResponse :: ModifyOptionGroupResult
modifyOptionGroupResponse = ModifyOptionGroupResult
    { _mogrOptionGroup = Nothing
    }

mogrOptionGroup :: Lens' ModifyOptionGroupResult (Maybe OptionGroup)
mogrOptionGroup = lens _mogrOptionGroup (\s a -> s { _mogrOptionGroup = a })

instance AWSRequest ModifyOptionGroupMessage where
    type Sv ModifyOptionGroupMessage = RDS
    type Rs ModifyOptionGroupMessage = ModifyOptionGroupResult

    request  = post "ModifyOptionGroup"
    response = xmlResponse $ \h x -> ModifyOptionGroupResult
        <$> x %| "OptionGroup"
