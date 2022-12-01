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
-- Module      : Amazonka.Connect.Types.SecurityProfileSearchSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.SecurityProfileSearchSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the returned security profiles.
--
-- /See:/ 'newSecurityProfileSearchSummary' smart constructor.
data SecurityProfileSearchSummary = SecurityProfileSearchSummary'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the security profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the security profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the security profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the security profile.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The organization resource identifier.
    organizationResourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityProfileSearchSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'securityProfileSearchSummary_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'arn', 'securityProfileSearchSummary_arn' - The Amazon Resource Name (ARN) of the security profile.
--
-- 'id', 'securityProfileSearchSummary_id' - The identifier of the security profile.
--
-- 'description', 'securityProfileSearchSummary_description' - The description of the security profile.
--
-- 'securityProfileName', 'securityProfileSearchSummary_securityProfileName' - The name of the security profile.
--
-- 'organizationResourceId', 'securityProfileSearchSummary_organizationResourceId' - The organization resource identifier.
newSecurityProfileSearchSummary ::
  SecurityProfileSearchSummary
newSecurityProfileSearchSummary =
  SecurityProfileSearchSummary'
    { tags =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      organizationResourceId = Prelude.Nothing
    }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
securityProfileSearchSummary_tags :: Lens.Lens' SecurityProfileSearchSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
securityProfileSearchSummary_tags = Lens.lens (\SecurityProfileSearchSummary' {tags} -> tags) (\s@SecurityProfileSearchSummary' {} a -> s {tags = a} :: SecurityProfileSearchSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the security profile.
securityProfileSearchSummary_arn :: Lens.Lens' SecurityProfileSearchSummary (Prelude.Maybe Prelude.Text)
securityProfileSearchSummary_arn = Lens.lens (\SecurityProfileSearchSummary' {arn} -> arn) (\s@SecurityProfileSearchSummary' {} a -> s {arn = a} :: SecurityProfileSearchSummary)

-- | The identifier of the security profile.
securityProfileSearchSummary_id :: Lens.Lens' SecurityProfileSearchSummary (Prelude.Maybe Prelude.Text)
securityProfileSearchSummary_id = Lens.lens (\SecurityProfileSearchSummary' {id} -> id) (\s@SecurityProfileSearchSummary' {} a -> s {id = a} :: SecurityProfileSearchSummary)

-- | The description of the security profile.
securityProfileSearchSummary_description :: Lens.Lens' SecurityProfileSearchSummary (Prelude.Maybe Prelude.Text)
securityProfileSearchSummary_description = Lens.lens (\SecurityProfileSearchSummary' {description} -> description) (\s@SecurityProfileSearchSummary' {} a -> s {description = a} :: SecurityProfileSearchSummary)

-- | The name of the security profile.
securityProfileSearchSummary_securityProfileName :: Lens.Lens' SecurityProfileSearchSummary (Prelude.Maybe Prelude.Text)
securityProfileSearchSummary_securityProfileName = Lens.lens (\SecurityProfileSearchSummary' {securityProfileName} -> securityProfileName) (\s@SecurityProfileSearchSummary' {} a -> s {securityProfileName = a} :: SecurityProfileSearchSummary)

-- | The organization resource identifier.
securityProfileSearchSummary_organizationResourceId :: Lens.Lens' SecurityProfileSearchSummary (Prelude.Maybe Prelude.Text)
securityProfileSearchSummary_organizationResourceId = Lens.lens (\SecurityProfileSearchSummary' {organizationResourceId} -> organizationResourceId) (\s@SecurityProfileSearchSummary' {} a -> s {organizationResourceId = a} :: SecurityProfileSearchSummary)

instance Core.FromJSON SecurityProfileSearchSummary where
  parseJSON =
    Core.withObject
      "SecurityProfileSearchSummary"
      ( \x ->
          SecurityProfileSearchSummary'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SecurityProfileName")
            Prelude.<*> (x Core..:? "OrganizationResourceId")
      )

instance
  Prelude.Hashable
    SecurityProfileSearchSummary
  where
  hashWithSalt _salt SecurityProfileSearchSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` securityProfileName
      `Prelude.hashWithSalt` organizationResourceId

instance Prelude.NFData SecurityProfileSearchSummary where
  rnf SecurityProfileSearchSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf organizationResourceId
