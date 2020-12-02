{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchRuleGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.PatchRule

-- | A set of rules defining the approval rules for a patch baseline.
--
--
--
-- /See:/ 'patchRuleGroup' smart constructor.
newtype PatchRuleGroup = PatchRuleGroup'
  { _prgPatchRules ::
      [PatchRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PatchRuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prgPatchRules' - The rules that make up the rule group.
patchRuleGroup ::
  PatchRuleGroup
patchRuleGroup = PatchRuleGroup' {_prgPatchRules = mempty}

-- | The rules that make up the rule group.
prgPatchRules :: Lens' PatchRuleGroup [PatchRule]
prgPatchRules = lens _prgPatchRules (\s a -> s {_prgPatchRules = a}) . _Coerce

instance FromJSON PatchRuleGroup where
  parseJSON =
    withObject
      "PatchRuleGroup"
      (\x -> PatchRuleGroup' <$> (x .:? "PatchRules" .!= mempty))

instance Hashable PatchRuleGroup

instance NFData PatchRuleGroup

instance ToJSON PatchRuleGroup where
  toJSON PatchRuleGroup' {..} =
    object (catMaybes [Just ("PatchRules" .= _prgPatchRules)])
