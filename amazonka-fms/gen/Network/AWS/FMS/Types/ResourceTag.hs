{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.FMS.Types.ResourceTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ResourceTag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The resource tags that AWS Firewall Manager uses to determine if a
-- particular resource should be included or excluded from the AWS Firewall
-- Manager policy. Tags enable you to categorize your AWS resources in
-- different ways, for example, by purpose, owner, or environment. Each tag
-- consists of a key and an optional value. Firewall Manager combines the
-- tags with \"AND\" so that, if you add more than one tag to a policy
-- scope, a resource must have all the specified tags to be included or
-- excluded. For more information, see
-- <https://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html Working with Tag Editor>.
--
-- /See:/ 'newResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | The resource tag value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The resource tag key.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceTag_value' - The resource tag value.
--
-- 'key', 'resourceTag_key' - The resource tag key.
newResourceTag ::
  -- | 'key'
  Prelude.Text ->
  ResourceTag
newResourceTag pKey_ =
  ResourceTag' {value = Prelude.Nothing, key = pKey_}

-- | The resource tag value.
resourceTag_value :: Lens.Lens' ResourceTag (Prelude.Maybe Prelude.Text)
resourceTag_value = Lens.lens (\ResourceTag' {value} -> value) (\s@ResourceTag' {} a -> s {value = a} :: ResourceTag)

-- | The resource tag key.
resourceTag_key :: Lens.Lens' ResourceTag Prelude.Text
resourceTag_key = Lens.lens (\ResourceTag' {key} -> key) (\s@ResourceTag' {} a -> s {key = a} :: ResourceTag)

instance Prelude.FromJSON ResourceTag where
  parseJSON =
    Prelude.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Prelude.<$> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..: "Key")
      )

instance Prelude.Hashable ResourceTag

instance Prelude.NFData ResourceTag

instance Prelude.ToJSON ResourceTag where
  toJSON ResourceTag' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Value" Prelude..=) Prelude.<$> value,
            Prelude.Just ("Key" Prelude..= key)
          ]
      )
