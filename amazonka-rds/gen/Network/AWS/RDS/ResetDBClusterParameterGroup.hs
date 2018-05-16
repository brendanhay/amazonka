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
-- Module      : Network.AWS.RDS.ResetDBClusterParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB cluster parameter group to the default value. To reset specific parameters submit a list of the following: @ParameterName@ and @ApplyMethod@ . To reset the entire DB cluster parameter group, specify the @DBClusterParameterGroupName@ and @ResetAllParameters@ parameters.
--
--
-- When resetting the entire group, dynamic parameters are updated immediately and static parameters are set to @pending-reboot@ to take effect on the next DB instance restart or 'RebootDBInstance' request. You must call 'RebootDBInstance' for every DB instance in your DB cluster that you want the updated static parameter to apply to.
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.ResetDBClusterParameterGroup
    (
    -- * Creating a Request
      resetDBClusterParameterGroup
    , ResetDBClusterParameterGroup
    -- * Request Lenses
    , rdcpgResetAllParameters
    , rdcpgParameters
    , rdcpgDBClusterParameterGroupName

    -- * Destructuring the Response
    , dbClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage
    -- * Response Lenses
    , dcpgnmDBClusterParameterGroupName
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'resetDBClusterParameterGroup' smart constructor.
data ResetDBClusterParameterGroup = ResetDBClusterParameterGroup'
  { _rdcpgResetAllParameters          :: !(Maybe Bool)
  , _rdcpgParameters                  :: !(Maybe [Parameter])
  , _rdcpgDBClusterParameterGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcpgResetAllParameters' - A value that is set to @true@ to reset all parameters in the DB cluster parameter group to their default values, and @false@ otherwise. You can't use this parameter if there is a list of parameter names specified for the @Parameters@ parameter.
--
-- * 'rdcpgParameters' - A list of parameter names in the DB cluster parameter group to reset to the default values. You can't use this parameter if the @ResetAllParameters@ parameter is set to @true@ .
--
-- * 'rdcpgDBClusterParameterGroupName' - The name of the DB cluster parameter group to reset.
resetDBClusterParameterGroup
    :: Text -- ^ 'rdcpgDBClusterParameterGroupName'
    -> ResetDBClusterParameterGroup
resetDBClusterParameterGroup pDBClusterParameterGroupName_ =
  ResetDBClusterParameterGroup'
    { _rdcpgResetAllParameters = Nothing
    , _rdcpgParameters = Nothing
    , _rdcpgDBClusterParameterGroupName = pDBClusterParameterGroupName_
    }


-- | A value that is set to @true@ to reset all parameters in the DB cluster parameter group to their default values, and @false@ otherwise. You can't use this parameter if there is a list of parameter names specified for the @Parameters@ parameter.
rdcpgResetAllParameters :: Lens' ResetDBClusterParameterGroup (Maybe Bool)
rdcpgResetAllParameters = lens _rdcpgResetAllParameters (\ s a -> s{_rdcpgResetAllParameters = a})

-- | A list of parameter names in the DB cluster parameter group to reset to the default values. You can't use this parameter if the @ResetAllParameters@ parameter is set to @true@ .
rdcpgParameters :: Lens' ResetDBClusterParameterGroup [Parameter]
rdcpgParameters = lens _rdcpgParameters (\ s a -> s{_rdcpgParameters = a}) . _Default . _Coerce

-- | The name of the DB cluster parameter group to reset.
rdcpgDBClusterParameterGroupName :: Lens' ResetDBClusterParameterGroup Text
rdcpgDBClusterParameterGroupName = lens _rdcpgDBClusterParameterGroupName (\ s a -> s{_rdcpgDBClusterParameterGroupName = a})

instance AWSRequest ResetDBClusterParameterGroup
         where
        type Rs ResetDBClusterParameterGroup =
             DBClusterParameterGroupNameMessage
        request = postQuery rds
        response
          = receiveXMLWrapper
              "ResetDBClusterParameterGroupResult"
              (\ s h x -> parseXML x)

instance Hashable ResetDBClusterParameterGroup where

instance NFData ResetDBClusterParameterGroup where

instance ToHeaders ResetDBClusterParameterGroup where
        toHeaders = const mempty

instance ToPath ResetDBClusterParameterGroup where
        toPath = const "/"

instance ToQuery ResetDBClusterParameterGroup where
        toQuery ResetDBClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ResetDBClusterParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ResetAllParameters" =: _rdcpgResetAllParameters,
               "Parameters" =:
                 toQuery
                   (toQueryList "Parameter" <$> _rdcpgParameters),
               "DBClusterParameterGroupName" =:
                 _rdcpgDBClusterParameterGroupName]
