{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationStringFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationStringFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The filters to describe or get information about your managed instances.
--
-- /See:/ 'newInstanceInformationStringFilter' smart constructor.
data InstanceInformationStringFilter = InstanceInformationStringFilter'
  { -- | The filter key name to describe your instances. For example:
    --
    -- \"InstanceIds\"|\"AgentVersion\"|\"PingStatus\"|\"PlatformTypes\"|\"ActivationIds\"|\"IamRole\"|\"ResourceType\"|\"AssociationStatus\"|\"Tag
    -- Key\"
    --
    -- @Tag key@ is not a valid filter. You must specify either @tag-key@ or
    -- @tag:keyname@ and a string. Here are some valid examples: tag-key,
    -- tag:123, tag:al!, tag:Windows. Here are some /invalid/ examples:
    -- tag-keys, Tag Key, tag:, tagKey, abc:keyname.
    key :: Prelude.Text,
    -- | The filter values.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceInformationStringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'instanceInformationStringFilter_key' - The filter key name to describe your instances. For example:
--
-- \"InstanceIds\"|\"AgentVersion\"|\"PingStatus\"|\"PlatformTypes\"|\"ActivationIds\"|\"IamRole\"|\"ResourceType\"|\"AssociationStatus\"|\"Tag
-- Key\"
--
-- @Tag key@ is not a valid filter. You must specify either @tag-key@ or
-- @tag:keyname@ and a string. Here are some valid examples: tag-key,
-- tag:123, tag:al!, tag:Windows. Here are some /invalid/ examples:
-- tag-keys, Tag Key, tag:, tagKey, abc:keyname.
--
-- 'values', 'instanceInformationStringFilter_values' - The filter values.
newInstanceInformationStringFilter ::
  -- | 'key'
  Prelude.Text ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  InstanceInformationStringFilter
newInstanceInformationStringFilter pKey_ pValues_ =
  InstanceInformationStringFilter'
    { key = pKey_,
      values = Lens._Coerce Lens.# pValues_
    }

-- | The filter key name to describe your instances. For example:
--
-- \"InstanceIds\"|\"AgentVersion\"|\"PingStatus\"|\"PlatformTypes\"|\"ActivationIds\"|\"IamRole\"|\"ResourceType\"|\"AssociationStatus\"|\"Tag
-- Key\"
--
-- @Tag key@ is not a valid filter. You must specify either @tag-key@ or
-- @tag:keyname@ and a string. Here are some valid examples: tag-key,
-- tag:123, tag:al!, tag:Windows. Here are some /invalid/ examples:
-- tag-keys, Tag Key, tag:, tagKey, abc:keyname.
instanceInformationStringFilter_key :: Lens.Lens' InstanceInformationStringFilter Prelude.Text
instanceInformationStringFilter_key = Lens.lens (\InstanceInformationStringFilter' {key} -> key) (\s@InstanceInformationStringFilter' {} a -> s {key = a} :: InstanceInformationStringFilter)

-- | The filter values.
instanceInformationStringFilter_values :: Lens.Lens' InstanceInformationStringFilter (Prelude.NonEmpty Prelude.Text)
instanceInformationStringFilter_values = Lens.lens (\InstanceInformationStringFilter' {values} -> values) (\s@InstanceInformationStringFilter' {} a -> s {values = a} :: InstanceInformationStringFilter) Prelude.. Lens._Coerce

instance
  Prelude.Hashable
    InstanceInformationStringFilter

instance
  Prelude.NFData
    InstanceInformationStringFilter

instance Core.ToJSON InstanceInformationStringFilter where
  toJSON InstanceInformationStringFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Values" Core..= values)
          ]
      )
