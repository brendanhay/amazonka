{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DeprecateDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deprecates the specified domain. After a domain has been deprecated it
-- cannot be used to create new workflow executions or register new types.
-- However, you can still use visibility actions on this domain. Deprecating a
-- domain also deprecates all activity and workflow types registered in the
-- domain. Executions that were started before the domain was deprecated will
-- continue to run. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. You cannot
-- use an IAM policy to constrain this action's parameters. If the caller does
-- not have sufficient permissions to invoke the action, or the parameter
-- values fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DeprecateDomain.html>
module Network.AWS.SWF.DeprecateDomain
    (
    -- * Request
      DeprecateDomain
    -- ** Request constructor
    , deprecateDomain
    -- ** Request lenses
    , dd1Name

    -- * Response
    , DeprecateDomainResponse
    -- ** Response constructor
    , deprecateDomainResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

newtype DeprecateDomain = DeprecateDomain
    { _dd1Name :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeprecateDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dd1Name' @::@ 'Text'
--
deprecateDomain :: Text -- ^ 'dd1Name'
                -> DeprecateDomain
deprecateDomain p1 = DeprecateDomain
    { _dd1Name = p1
    }

-- | The name of the domain to deprecate.
dd1Name :: Lens' DeprecateDomain Text
dd1Name = lens _dd1Name (\s a -> s { _dd1Name = a })

data DeprecateDomainResponse = DeprecateDomainResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeprecateDomainResponse' constructor.
deprecateDomainResponse :: DeprecateDomainResponse
deprecateDomainResponse = DeprecateDomainResponse

instance ToPath DeprecateDomain where
    toPath = const "/"

instance ToQuery DeprecateDomain where
    toQuery = const mempty

instance ToHeaders DeprecateDomain

instance ToJSON DeprecateDomain where
    toJSON DeprecateDomain{..} = object
        [ "name" .= _dd1Name
        ]

instance AWSRequest DeprecateDomain where
    type Sv DeprecateDomain = SWF
    type Rs DeprecateDomain = DeprecateDomainResponse

    request  = post "DeprecateDomain"
    response = nullResponse DeprecateDomainResponse
