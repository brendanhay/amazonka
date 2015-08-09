{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBClusterParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB cluster parameter group. To modify more
-- than one parameter, submit a list of the following: @ParameterName@,
-- @ParameterValue@, and @ApplyMethod@. A maximum of 20 parameters can be
-- modified in a single request.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- Changes to dynamic parameters are applied immediately. Changes to static
-- parameters require a reboot without failover to the DB cluster
-- associated with the parameter group before the change can take effect.
--
-- After you create a DB cluster parameter group, you should wait at least
-- 5 minutes before creating your first DB cluster that uses that DB
-- cluster parameter group as the default parameter group. This allows
-- Amazon RDS to fully complete the create action before the parameter
-- group is used as the default for a new DB cluster. This is especially
-- important for parameters that are critical when creating the default
-- database for a DB cluster, such as the character set for the default
-- database defined by the @character_set_database@ parameter. You can use
-- the /Parameter Groups/ option of the
-- <https://console.aws.amazon.com/rds/ Amazon RDS console> or the
-- DescribeDBClusterParameters command to verify that your DB cluster
-- parameter group has been created or modified.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyDBClusterParameterGroup.html AWS API Reference> for ModifyDBClusterParameterGroup.
module Network.AWS.RDS.ModifyDBClusterParameterGroup
    (
    -- * Creating a Request
      ModifyDBClusterParameterGroup
    , modifyDBClusterParameterGroup
    -- * Request Lenses
    , mdcpgDBClusterParameterGroupName
    , mdcpgParameters

    -- * Destructuring the Response
    , DBClusterParameterGroupNameMessage
    , dbClusterParameterGroupNameMessage
    -- * Response Lenses
    , dcpgnmDBClusterParameterGroupName
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyDBClusterParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdcpgDBClusterParameterGroupName'
--
-- * 'mdcpgParameters'
data ModifyDBClusterParameterGroup = ModifyDBClusterParameterGroup'
    { _mdcpgDBClusterParameterGroupName :: !Text
    , _mdcpgParameters                  :: ![Parameter]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyDBClusterParameterGroup' smart constructor.
modifyDBClusterParameterGroup :: Text -> ModifyDBClusterParameterGroup
modifyDBClusterParameterGroup pDBClusterParameterGroupName_ =
    ModifyDBClusterParameterGroup'
    { _mdcpgDBClusterParameterGroupName = pDBClusterParameterGroupName_
    , _mdcpgParameters = mempty
    }

-- | The name of the DB cluster parameter group to modify.
mdcpgDBClusterParameterGroupName :: Lens' ModifyDBClusterParameterGroup Text
mdcpgDBClusterParameterGroupName = lens _mdcpgDBClusterParameterGroupName (\ s a -> s{_mdcpgDBClusterParameterGroupName = a});

-- | A list of parameters in the DB cluster parameter group to modify.
mdcpgParameters :: Lens' ModifyDBClusterParameterGroup [Parameter]
mdcpgParameters = lens _mdcpgParameters (\ s a -> s{_mdcpgParameters = a}) . _Coerce;

instance AWSRequest ModifyDBClusterParameterGroup
         where
        type Sv ModifyDBClusterParameterGroup = RDS
        type Rs ModifyDBClusterParameterGroup =
             DBClusterParameterGroupNameMessage
        request = postQuery
        response
          = receiveXMLWrapper
              "ModifyDBClusterParameterGroupResult"
              (\ s h x -> parseXML x)

instance ToHeaders ModifyDBClusterParameterGroup
         where
        toHeaders = const mempty

instance ToPath ModifyDBClusterParameterGroup where
        toPath = const "/"

instance ToQuery ModifyDBClusterParameterGroup where
        toQuery ModifyDBClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyDBClusterParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterParameterGroupName" =:
                 _mdcpgDBClusterParameterGroupName,
               "Parameters" =:
                 toQueryList "Parameter" _mdcpgParameters]
