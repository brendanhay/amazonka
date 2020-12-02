{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackProperty where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.AttackLayer
import Network.AWS.Shield.Types.AttackPropertyIdentifier
import Network.AWS.Shield.Types.Contributor
import Network.AWS.Shield.Types.Unit

-- | Details of the described attack.
--
--
--
-- /See:/ 'attackProperty' smart constructor.
data AttackProperty = AttackProperty'
  { _apAttackLayer ::
      !(Maybe AttackLayer),
    _apTopContributors :: !(Maybe [Contributor]),
    _apAttackPropertyIdentifier ::
      !(Maybe AttackPropertyIdentifier),
    _apTotal :: !(Maybe Integer),
    _apUnit :: !(Maybe Unit)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttackProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apAttackLayer' - The type of distributed denial of service (DDoS) event that was observed. @NETWORK@ indicates layer 3 and layer 4 events and @APPLICATION@ indicates layer 7 events.
--
-- * 'apTopContributors' - The array of contributor objects that includes the top five contributors to an attack.
--
-- * 'apAttackPropertyIdentifier' - Defines the DDoS attack property information that is provided. The @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values are valid only for WordPress reflective pingback DDoS attacks.
--
-- * 'apTotal' - The total contributions made to this attack by all contributors, not just the five listed in the @TopContributors@ list.
--
-- * 'apUnit' - The unit of the @Value@ of the contributions.
attackProperty ::
  AttackProperty
attackProperty =
  AttackProperty'
    { _apAttackLayer = Nothing,
      _apTopContributors = Nothing,
      _apAttackPropertyIdentifier = Nothing,
      _apTotal = Nothing,
      _apUnit = Nothing
    }

-- | The type of distributed denial of service (DDoS) event that was observed. @NETWORK@ indicates layer 3 and layer 4 events and @APPLICATION@ indicates layer 7 events.
apAttackLayer :: Lens' AttackProperty (Maybe AttackLayer)
apAttackLayer = lens _apAttackLayer (\s a -> s {_apAttackLayer = a})

-- | The array of contributor objects that includes the top five contributors to an attack.
apTopContributors :: Lens' AttackProperty [Contributor]
apTopContributors = lens _apTopContributors (\s a -> s {_apTopContributors = a}) . _Default . _Coerce

-- | Defines the DDoS attack property information that is provided. The @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values are valid only for WordPress reflective pingback DDoS attacks.
apAttackPropertyIdentifier :: Lens' AttackProperty (Maybe AttackPropertyIdentifier)
apAttackPropertyIdentifier = lens _apAttackPropertyIdentifier (\s a -> s {_apAttackPropertyIdentifier = a})

-- | The total contributions made to this attack by all contributors, not just the five listed in the @TopContributors@ list.
apTotal :: Lens' AttackProperty (Maybe Integer)
apTotal = lens _apTotal (\s a -> s {_apTotal = a})

-- | The unit of the @Value@ of the contributions.
apUnit :: Lens' AttackProperty (Maybe Unit)
apUnit = lens _apUnit (\s a -> s {_apUnit = a})

instance FromJSON AttackProperty where
  parseJSON =
    withObject
      "AttackProperty"
      ( \x ->
          AttackProperty'
            <$> (x .:? "AttackLayer")
            <*> (x .:? "TopContributors" .!= mempty)
            <*> (x .:? "AttackPropertyIdentifier")
            <*> (x .:? "Total")
            <*> (x .:? "Unit")
      )

instance Hashable AttackProperty

instance NFData AttackProperty
