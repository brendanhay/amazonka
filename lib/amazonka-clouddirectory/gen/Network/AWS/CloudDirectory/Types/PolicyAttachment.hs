{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.PolicyAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.PolicyAttachment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the @PolicyType@ , @PolicyId@ , and the @ObjectIdentifier@ to which it is attached. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
--
--
-- /See:/ 'policyAttachment' smart constructor.
data PolicyAttachment = PolicyAttachment'
  { _paPolicyId ::
      !(Maybe Text),
    _paPolicyType :: !(Maybe Text),
    _paObjectIdentifier :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paPolicyId' - The ID of @PolicyAttachment@ .
--
-- * 'paPolicyType' - The type of policy that can be associated with @PolicyAttachment@ .
--
-- * 'paObjectIdentifier' - The @ObjectIdentifier@ that is associated with @PolicyAttachment@ .
policyAttachment ::
  PolicyAttachment
policyAttachment =
  PolicyAttachment'
    { _paPolicyId = Nothing,
      _paPolicyType = Nothing,
      _paObjectIdentifier = Nothing
    }

-- | The ID of @PolicyAttachment@ .
paPolicyId :: Lens' PolicyAttachment (Maybe Text)
paPolicyId = lens _paPolicyId (\s a -> s {_paPolicyId = a})

-- | The type of policy that can be associated with @PolicyAttachment@ .
paPolicyType :: Lens' PolicyAttachment (Maybe Text)
paPolicyType = lens _paPolicyType (\s a -> s {_paPolicyType = a})

-- | The @ObjectIdentifier@ that is associated with @PolicyAttachment@ .
paObjectIdentifier :: Lens' PolicyAttachment (Maybe Text)
paObjectIdentifier = lens _paObjectIdentifier (\s a -> s {_paObjectIdentifier = a})

instance FromJSON PolicyAttachment where
  parseJSON =
    withObject
      "PolicyAttachment"
      ( \x ->
          PolicyAttachment'
            <$> (x .:? "PolicyId")
            <*> (x .:? "PolicyType")
            <*> (x .:? "ObjectIdentifier")
      )

instance Hashable PolicyAttachment

instance NFData PolicyAttachment
