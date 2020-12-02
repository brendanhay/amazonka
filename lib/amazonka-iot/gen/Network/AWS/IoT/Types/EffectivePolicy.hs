{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EffectivePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.EffectivePolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The policy that has the effect on the authorization results.
--
--
--
-- /See:/ 'effectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { _epPolicyName ::
      !(Maybe Text),
    _epPolicyDocument :: !(Maybe Text),
    _epPolicyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EffectivePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epPolicyName' - The policy name.
--
-- * 'epPolicyDocument' - The IAM policy document.
--
-- * 'epPolicyARN' - The policy ARN.
effectivePolicy ::
  EffectivePolicy
effectivePolicy =
  EffectivePolicy'
    { _epPolicyName = Nothing,
      _epPolicyDocument = Nothing,
      _epPolicyARN = Nothing
    }

-- | The policy name.
epPolicyName :: Lens' EffectivePolicy (Maybe Text)
epPolicyName = lens _epPolicyName (\s a -> s {_epPolicyName = a})

-- | The IAM policy document.
epPolicyDocument :: Lens' EffectivePolicy (Maybe Text)
epPolicyDocument = lens _epPolicyDocument (\s a -> s {_epPolicyDocument = a})

-- | The policy ARN.
epPolicyARN :: Lens' EffectivePolicy (Maybe Text)
epPolicyARN = lens _epPolicyARN (\s a -> s {_epPolicyARN = a})

instance FromJSON EffectivePolicy where
  parseJSON =
    withObject
      "EffectivePolicy"
      ( \x ->
          EffectivePolicy'
            <$> (x .:? "policyName")
            <*> (x .:? "policyDocument")
            <*> (x .:? "policyArn")
      )

instance Hashable EffectivePolicy

instance NFData EffectivePolicy
