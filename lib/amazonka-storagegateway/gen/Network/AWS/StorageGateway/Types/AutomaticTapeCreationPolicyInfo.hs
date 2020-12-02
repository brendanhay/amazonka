{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule

-- | Information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
--
--
--
-- /See:/ 'automaticTapeCreationPolicyInfo' smart constructor.
data AutomaticTapeCreationPolicyInfo = AutomaticTapeCreationPolicyInfo'
  { _atcpiGatewayARN ::
      !(Maybe Text),
    _atcpiAutomaticTapeCreationRules ::
      !( Maybe
           ( List1
               AutomaticTapeCreationRule
           )
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutomaticTapeCreationPolicyInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atcpiGatewayARN' - Undocumented member.
--
-- * 'atcpiAutomaticTapeCreationRules' - An automatic tape creation policy consists of a list of automatic tape creation rules. This returns the rules that determine when and how to automatically create new tapes.
automaticTapeCreationPolicyInfo ::
  AutomaticTapeCreationPolicyInfo
automaticTapeCreationPolicyInfo =
  AutomaticTapeCreationPolicyInfo'
    { _atcpiGatewayARN = Nothing,
      _atcpiAutomaticTapeCreationRules = Nothing
    }

-- | Undocumented member.
atcpiGatewayARN :: Lens' AutomaticTapeCreationPolicyInfo (Maybe Text)
atcpiGatewayARN = lens _atcpiGatewayARN (\s a -> s {_atcpiGatewayARN = a})

-- | An automatic tape creation policy consists of a list of automatic tape creation rules. This returns the rules that determine when and how to automatically create new tapes.
atcpiAutomaticTapeCreationRules :: Lens' AutomaticTapeCreationPolicyInfo (Maybe (NonEmpty AutomaticTapeCreationRule))
atcpiAutomaticTapeCreationRules = lens _atcpiAutomaticTapeCreationRules (\s a -> s {_atcpiAutomaticTapeCreationRules = a}) . mapping _List1

instance FromJSON AutomaticTapeCreationPolicyInfo where
  parseJSON =
    withObject
      "AutomaticTapeCreationPolicyInfo"
      ( \x ->
          AutomaticTapeCreationPolicyInfo'
            <$> (x .:? "GatewayARN") <*> (x .:? "AutomaticTapeCreationRules")
      )

instance Hashable AutomaticTapeCreationPolicyInfo

instance NFData AutomaticTapeCreationPolicyInfo
