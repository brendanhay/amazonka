{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ResetClusterParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets one or more parameters of the specified parameter group to their
-- default values and sets the source values of the parameters to
-- \"engine-default\". To reset the entire parameter group specify the
-- /ResetAllParameters/ parameter. For parameter changes to take effect you
-- must reboot any associated clusters.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ResetClusterParameterGroup.html>
module Network.AWS.Redshift.ResetClusterParameterGroup
    (
    -- * Request
      ResetClusterParameterGroup
    -- ** Request constructor
    , resetClusterParameterGroup
    -- ** Request lenses
    , rcpgrqResetAllParameters
    , rcpgrqParameters
    , rcpgrqParameterGroupName

    -- * Response
    , ClusterParameterGroupNameMessage
    -- ** Response constructor
    , clusterParameterGroupNameMessage
    -- ** Response lenses
    , cpgnmParameterGroupStatus
    , cpgnmParameterGroupName
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'resetClusterParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcpgrqResetAllParameters'
--
-- * 'rcpgrqParameters'
--
-- * 'rcpgrqParameterGroupName'
data ResetClusterParameterGroup = ResetClusterParameterGroup'
    { _rcpgrqResetAllParameters :: !(Maybe Bool)
    , _rcpgrqParameters         :: !(Maybe [Parameter])
    , _rcpgrqParameterGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResetClusterParameterGroup' smart constructor.
resetClusterParameterGroup :: Text -> ResetClusterParameterGroup
resetClusterParameterGroup pParameterGroupName =
    ResetClusterParameterGroup'
    { _rcpgrqResetAllParameters = Nothing
    , _rcpgrqParameters = Nothing
    , _rcpgrqParameterGroupName = pParameterGroupName
    }

-- | If @true@, all parameters in the specified parameter group will be reset
-- to their default values.
--
-- Default: @true@
rcpgrqResetAllParameters :: Lens' ResetClusterParameterGroup (Maybe Bool)
rcpgrqResetAllParameters = lens _rcpgrqResetAllParameters (\ s a -> s{_rcpgrqResetAllParameters = a});

-- | An array of names of parameters to be reset. If /ResetAllParameters/
-- option is not used, then at least one parameter name must be supplied.
--
-- Constraints: A maximum of 20 parameters can be reset in a single
-- request.
rcpgrqParameters :: Lens' ResetClusterParameterGroup [Parameter]
rcpgrqParameters = lens _rcpgrqParameters (\ s a -> s{_rcpgrqParameters = a}) . _Default;

-- | The name of the cluster parameter group to be reset.
rcpgrqParameterGroupName :: Lens' ResetClusterParameterGroup Text
rcpgrqParameterGroupName = lens _rcpgrqParameterGroupName (\ s a -> s{_rcpgrqParameterGroupName = a});

instance AWSRequest ResetClusterParameterGroup where
        type Sv ResetClusterParameterGroup = Redshift
        type Rs ResetClusterParameterGroup =
             ClusterParameterGroupNameMessage
        request = post
        response
          = receiveXMLWrapper
              "ResetClusterParameterGroupResult"
              (\ s h x -> parseXML x)

instance ToHeaders ResetClusterParameterGroup where
        toHeaders = const mempty

instance ToPath ResetClusterParameterGroup where
        toPath = const "/"

instance ToQuery ResetClusterParameterGroup where
        toQuery ResetClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ResetClusterParameterGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ResetAllParameters" =: _rcpgrqResetAllParameters,
               "Parameters" =:
                 toQuery
                   (toQueryList "Parameter" <$> _rcpgrqParameters),
               "ParameterGroupName" =: _rcpgrqParameterGroupName]
