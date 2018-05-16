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
-- Module      : Network.AWS.RDS.ModifyDBParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB parameter group. To modify more than one parameter, submit a list of the following: @ParameterName@ , @ParameterValue@ , and @ApplyMethod@ . A maximum of 20 parameters can be modified in a single request.
--
--
-- /Important:/ After you modify a DB parameter group, you should wait at least 5 minutes before creating your first DB instance that uses that DB parameter group as the default parameter group. This allows Amazon RDS to fully complete the modify action before the parameter group is used as the default for a new DB instance. This is especially important for parameters that are critical when creating the default database for a DB instance, such as the character set for the default database defined by the @character_set_database@ parameter. You can use the /Parameter Groups/ option of the <https://console.aws.amazon.com/rds/ Amazon RDS console> or the /DescribeDBParameters/ command to verify that your DB parameter group has been created or modified.
--
module Network.AWS.RDS.ModifyDBParameterGroup
    (
    -- * Creating a Request
      modifyDBParameterGroup
    , ModifyDBParameterGroup
    -- * Request Lenses
    , mdpgDBParameterGroupName
    , mdpgParameters

    -- * Destructuring the Response
    , dbParameterGroupNameMessage
    , DBParameterGroupNameMessage
    -- * Response Lenses
    , dpgnmDBParameterGroupName
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
-- /See:/ 'modifyDBParameterGroup' smart constructor.
data ModifyDBParameterGroup = ModifyDBParameterGroup'
  { _mdpgDBParameterGroupName :: !Text
  , _mdpgParameters           :: ![Parameter]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdpgDBParameterGroupName' - The name of the DB parameter group. Constraints:     * If supplied, must match the name of an existing DBParameterGroup.
--
-- * 'mdpgParameters' - An array of parameter names, values, and the apply method for the parameter update. At least one parameter name, value, and apply method must be supplied; subsequent arguments are optional. A maximum of 20 parameters can be modified in a single request. Valid Values (for the application method): @immediate | pending-reboot@
modifyDBParameterGroup
    :: Text -- ^ 'mdpgDBParameterGroupName'
    -> ModifyDBParameterGroup
modifyDBParameterGroup pDBParameterGroupName_ =
  ModifyDBParameterGroup'
    { _mdpgDBParameterGroupName = pDBParameterGroupName_
    , _mdpgParameters = mempty
    }


-- | The name of the DB parameter group. Constraints:     * If supplied, must match the name of an existing DBParameterGroup.
mdpgDBParameterGroupName :: Lens' ModifyDBParameterGroup Text
mdpgDBParameterGroupName = lens _mdpgDBParameterGroupName (\ s a -> s{_mdpgDBParameterGroupName = a})

-- | An array of parameter names, values, and the apply method for the parameter update. At least one parameter name, value, and apply method must be supplied; subsequent arguments are optional. A maximum of 20 parameters can be modified in a single request. Valid Values (for the application method): @immediate | pending-reboot@
mdpgParameters :: Lens' ModifyDBParameterGroup [Parameter]
mdpgParameters = lens _mdpgParameters (\ s a -> s{_mdpgParameters = a}) . _Coerce

instance AWSRequest ModifyDBParameterGroup where
        type Rs ModifyDBParameterGroup =
             DBParameterGroupNameMessage
        request = postQuery rds
        response
          = receiveXMLWrapper "ModifyDBParameterGroupResult"
              (\ s h x -> parseXML x)

instance Hashable ModifyDBParameterGroup where

instance NFData ModifyDBParameterGroup where

instance ToHeaders ModifyDBParameterGroup where
        toHeaders = const mempty

instance ToPath ModifyDBParameterGroup where
        toPath = const "/"

instance ToQuery ModifyDBParameterGroup where
        toQuery ModifyDBParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyDBParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBParameterGroupName" =: _mdpgDBParameterGroupName,
               "Parameters" =:
                 toQueryList "Parameter" _mdpgParameters]
