{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GluePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GluePolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure for returning a resource policy.
--
--
--
-- /See:/ 'gluePolicy' smart constructor.
data GluePolicy = GluePolicy'
  { _gpPolicyInJSON :: !(Maybe Text),
    _gpUpdateTime :: !(Maybe POSIX),
    _gpPolicyHash :: !(Maybe Text),
    _gpCreateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GluePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpPolicyInJSON' - Contains the requested policy document, in JSON format.
--
-- * 'gpUpdateTime' - The date and time at which the policy was last updated.
--
-- * 'gpPolicyHash' - Contains the hash value associated with this policy.
--
-- * 'gpCreateTime' - The date and time at which the policy was created.
gluePolicy ::
  GluePolicy
gluePolicy =
  GluePolicy'
    { _gpPolicyInJSON = Nothing,
      _gpUpdateTime = Nothing,
      _gpPolicyHash = Nothing,
      _gpCreateTime = Nothing
    }

-- | Contains the requested policy document, in JSON format.
gpPolicyInJSON :: Lens' GluePolicy (Maybe Text)
gpPolicyInJSON = lens _gpPolicyInJSON (\s a -> s {_gpPolicyInJSON = a})

-- | The date and time at which the policy was last updated.
gpUpdateTime :: Lens' GluePolicy (Maybe UTCTime)
gpUpdateTime = lens _gpUpdateTime (\s a -> s {_gpUpdateTime = a}) . mapping _Time

-- | Contains the hash value associated with this policy.
gpPolicyHash :: Lens' GluePolicy (Maybe Text)
gpPolicyHash = lens _gpPolicyHash (\s a -> s {_gpPolicyHash = a})

-- | The date and time at which the policy was created.
gpCreateTime :: Lens' GluePolicy (Maybe UTCTime)
gpCreateTime = lens _gpCreateTime (\s a -> s {_gpCreateTime = a}) . mapping _Time

instance FromJSON GluePolicy where
  parseJSON =
    withObject
      "GluePolicy"
      ( \x ->
          GluePolicy'
            <$> (x .:? "PolicyInJson")
            <*> (x .:? "UpdateTime")
            <*> (x .:? "PolicyHash")
            <*> (x .:? "CreateTime")
      )

instance Hashable GluePolicy

instance NFData GluePolicy
