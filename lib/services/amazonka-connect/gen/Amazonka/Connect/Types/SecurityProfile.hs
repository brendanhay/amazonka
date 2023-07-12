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
-- Module      : Amazonka.Connect.Types.SecurityProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.SecurityProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a security profile.
--
-- /See:/ 'newSecurityProfile' smart constructor.
data SecurityProfile = SecurityProfile'
  { -- | The list of tags that a security profile uses to restrict access to
    -- resources in Amazon Connect.
    allowedAccessControlTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) for the secruity profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the security profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the security profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The organization resource identifier for the security profile.
    organizationResourceId :: Prelude.Maybe Prelude.Text,
    -- | The name for the security profile.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The list of resources that a security profile applies tag restrictions
    -- to in Amazon Connect.
    tagRestrictedResources :: Prelude.Maybe [Prelude.Text],
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedAccessControlTags', 'securityProfile_allowedAccessControlTags' - The list of tags that a security profile uses to restrict access to
-- resources in Amazon Connect.
--
-- 'arn', 'securityProfile_arn' - The Amazon Resource Name (ARN) for the secruity profile.
--
-- 'description', 'securityProfile_description' - The description of the security profile.
--
-- 'id', 'securityProfile_id' - The identifier for the security profile.
--
-- 'organizationResourceId', 'securityProfile_organizationResourceId' - The organization resource identifier for the security profile.
--
-- 'securityProfileName', 'securityProfile_securityProfileName' - The name for the security profile.
--
-- 'tagRestrictedResources', 'securityProfile_tagRestrictedResources' - The list of resources that a security profile applies tag restrictions
-- to in Amazon Connect.
--
-- 'tags', 'securityProfile_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
newSecurityProfile ::
  SecurityProfile
newSecurityProfile =
  SecurityProfile'
    { allowedAccessControlTags =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      organizationResourceId = Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      tagRestrictedResources = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The list of tags that a security profile uses to restrict access to
-- resources in Amazon Connect.
securityProfile_allowedAccessControlTags :: Lens.Lens' SecurityProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
securityProfile_allowedAccessControlTags = Lens.lens (\SecurityProfile' {allowedAccessControlTags} -> allowedAccessControlTags) (\s@SecurityProfile' {} a -> s {allowedAccessControlTags = a} :: SecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the secruity profile.
securityProfile_arn :: Lens.Lens' SecurityProfile (Prelude.Maybe Prelude.Text)
securityProfile_arn = Lens.lens (\SecurityProfile' {arn} -> arn) (\s@SecurityProfile' {} a -> s {arn = a} :: SecurityProfile)

-- | The description of the security profile.
securityProfile_description :: Lens.Lens' SecurityProfile (Prelude.Maybe Prelude.Text)
securityProfile_description = Lens.lens (\SecurityProfile' {description} -> description) (\s@SecurityProfile' {} a -> s {description = a} :: SecurityProfile)

-- | The identifier for the security profile.
securityProfile_id :: Lens.Lens' SecurityProfile (Prelude.Maybe Prelude.Text)
securityProfile_id = Lens.lens (\SecurityProfile' {id} -> id) (\s@SecurityProfile' {} a -> s {id = a} :: SecurityProfile)

-- | The organization resource identifier for the security profile.
securityProfile_organizationResourceId :: Lens.Lens' SecurityProfile (Prelude.Maybe Prelude.Text)
securityProfile_organizationResourceId = Lens.lens (\SecurityProfile' {organizationResourceId} -> organizationResourceId) (\s@SecurityProfile' {} a -> s {organizationResourceId = a} :: SecurityProfile)

-- | The name for the security profile.
securityProfile_securityProfileName :: Lens.Lens' SecurityProfile (Prelude.Maybe Prelude.Text)
securityProfile_securityProfileName = Lens.lens (\SecurityProfile' {securityProfileName} -> securityProfileName) (\s@SecurityProfile' {} a -> s {securityProfileName = a} :: SecurityProfile)

-- | The list of resources that a security profile applies tag restrictions
-- to in Amazon Connect.
securityProfile_tagRestrictedResources :: Lens.Lens' SecurityProfile (Prelude.Maybe [Prelude.Text])
securityProfile_tagRestrictedResources = Lens.lens (\SecurityProfile' {tagRestrictedResources} -> tagRestrictedResources) (\s@SecurityProfile' {} a -> s {tagRestrictedResources = a} :: SecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
securityProfile_tags :: Lens.Lens' SecurityProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
securityProfile_tags = Lens.lens (\SecurityProfile' {tags} -> tags) (\s@SecurityProfile' {} a -> s {tags = a} :: SecurityProfile) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SecurityProfile where
  parseJSON =
    Data.withObject
      "SecurityProfile"
      ( \x ->
          SecurityProfile'
            Prelude.<$> ( x
                            Data..:? "AllowedAccessControlTags"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "OrganizationResourceId")
            Prelude.<*> (x Data..:? "SecurityProfileName")
            Prelude.<*> ( x
                            Data..:? "TagRestrictedResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SecurityProfile where
  hashWithSalt _salt SecurityProfile' {..} =
    _salt
      `Prelude.hashWithSalt` allowedAccessControlTags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` organizationResourceId
      `Prelude.hashWithSalt` securityProfileName
      `Prelude.hashWithSalt` tagRestrictedResources
      `Prelude.hashWithSalt` tags

instance Prelude.NFData SecurityProfile where
  rnf SecurityProfile' {..} =
    Prelude.rnf allowedAccessControlTags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf organizationResourceId
      `Prelude.seq` Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf tagRestrictedResources
      `Prelude.seq` Prelude.rnf tags
