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
-- Module      : Network.AWS.Redshift.ResetClusterParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets one or more parameters of the specified parameter group to their default values and sets the source values of the parameters to "engine-default". To reset the entire parameter group specify the /ResetAllParameters/ parameter. For parameter changes to take effect you must reboot any associated clusters.
--
--
module Network.AWS.Redshift.ResetClusterParameterGroup
    (
    -- * Creating a Request
      resetClusterParameterGroup
    , ResetClusterParameterGroup
    -- * Request Lenses
    , rcpgResetAllParameters
    , rcpgParameters
    , rcpgParameterGroupName

    -- * Destructuring the Response
    , clusterParameterGroupNameMessage
    , ClusterParameterGroupNameMessage
    -- * Response Lenses
    , cpgnmParameterGroupStatus
    , cpgnmParameterGroupName
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'resetClusterParameterGroup' smart constructor.
data ResetClusterParameterGroup = ResetClusterParameterGroup'
  { _rcpgResetAllParameters :: !(Maybe Bool)
  , _rcpgParameters         :: !(Maybe [Parameter])
  , _rcpgParameterGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcpgResetAllParameters' - If @true@ , all parameters in the specified parameter group will be reset to their default values.  Default: @true@
--
-- * 'rcpgParameters' - An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied.  Constraints: A maximum of 20 parameters can be reset in a single request.
--
-- * 'rcpgParameterGroupName' - The name of the cluster parameter group to be reset.
resetClusterParameterGroup
    :: Text -- ^ 'rcpgParameterGroupName'
    -> ResetClusterParameterGroup
resetClusterParameterGroup pParameterGroupName_ =
  ResetClusterParameterGroup'
    { _rcpgResetAllParameters = Nothing
    , _rcpgParameters = Nothing
    , _rcpgParameterGroupName = pParameterGroupName_
    }


-- | If @true@ , all parameters in the specified parameter group will be reset to their default values.  Default: @true@
rcpgResetAllParameters :: Lens' ResetClusterParameterGroup (Maybe Bool)
rcpgResetAllParameters = lens _rcpgResetAllParameters (\ s a -> s{_rcpgResetAllParameters = a})

-- | An array of names of parameters to be reset. If /ResetAllParameters/ option is not used, then at least one parameter name must be supplied.  Constraints: A maximum of 20 parameters can be reset in a single request.
rcpgParameters :: Lens' ResetClusterParameterGroup [Parameter]
rcpgParameters = lens _rcpgParameters (\ s a -> s{_rcpgParameters = a}) . _Default . _Coerce

-- | The name of the cluster parameter group to be reset.
rcpgParameterGroupName :: Lens' ResetClusterParameterGroup Text
rcpgParameterGroupName = lens _rcpgParameterGroupName (\ s a -> s{_rcpgParameterGroupName = a})

instance AWSRequest ResetClusterParameterGroup where
        type Rs ResetClusterParameterGroup =
             ClusterParameterGroupNameMessage
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "ResetClusterParameterGroupResult"
              (\ s h x -> parseXML x)

instance Hashable ResetClusterParameterGroup where

instance NFData ResetClusterParameterGroup where

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
               "ResetAllParameters" =: _rcpgResetAllParameters,
               "Parameters" =:
                 toQuery
                   (toQueryList "Parameter" <$> _rcpgParameters),
               "ParameterGroupName" =: _rcpgParameterGroupName]
