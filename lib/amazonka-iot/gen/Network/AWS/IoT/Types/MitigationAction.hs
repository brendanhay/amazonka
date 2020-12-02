{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationAction where

import Network.AWS.IoT.Types.MitigationActionParams
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes which changes should be applied as part of a mitigation action.
--
--
--
-- /See:/ 'mitigationAction' smart constructor.
data MitigationAction = MitigationAction'
  { _maActionParams ::
      !(Maybe MitigationActionParams),
    _maName :: !(Maybe Text),
    _maId :: !(Maybe Text),
    _maRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MitigationAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maActionParams' - The set of parameters for this mitigation action. The parameters vary, depending on the kind of action you apply.
--
-- * 'maName' - A user-friendly name for the mitigation action.
--
-- * 'maId' - A unique identifier for the mitigation action.
--
-- * 'maRoleARN' - The IAM role ARN used to apply this mitigation action.
mitigationAction ::
  MitigationAction
mitigationAction =
  MitigationAction'
    { _maActionParams = Nothing,
      _maName = Nothing,
      _maId = Nothing,
      _maRoleARN = Nothing
    }

-- | The set of parameters for this mitigation action. The parameters vary, depending on the kind of action you apply.
maActionParams :: Lens' MitigationAction (Maybe MitigationActionParams)
maActionParams = lens _maActionParams (\s a -> s {_maActionParams = a})

-- | A user-friendly name for the mitigation action.
maName :: Lens' MitigationAction (Maybe Text)
maName = lens _maName (\s a -> s {_maName = a})

-- | A unique identifier for the mitigation action.
maId :: Lens' MitigationAction (Maybe Text)
maId = lens _maId (\s a -> s {_maId = a})

-- | The IAM role ARN used to apply this mitigation action.
maRoleARN :: Lens' MitigationAction (Maybe Text)
maRoleARN = lens _maRoleARN (\s a -> s {_maRoleARN = a})

instance FromJSON MitigationAction where
  parseJSON =
    withObject
      "MitigationAction"
      ( \x ->
          MitigationAction'
            <$> (x .:? "actionParams")
            <*> (x .:? "name")
            <*> (x .:? "id")
            <*> (x .:? "roleArn")
      )

instance Hashable MitigationAction

instance NFData MitigationAction
