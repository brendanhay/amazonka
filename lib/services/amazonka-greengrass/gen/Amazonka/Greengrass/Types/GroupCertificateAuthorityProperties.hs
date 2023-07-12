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
-- Module      : Amazonka.Greengrass.Types.GroupCertificateAuthorityProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.GroupCertificateAuthorityProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a certificate authority for a group.
--
-- /See:/ 'newGroupCertificateAuthorityProperties' smart constructor.
data GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties'
  { -- | The ARN of the certificate authority for the group.
    groupCertificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate authority for the group.
    groupCertificateAuthorityId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupCertificateAuthorityProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupCertificateAuthorityArn', 'groupCertificateAuthorityProperties_groupCertificateAuthorityArn' - The ARN of the certificate authority for the group.
--
-- 'groupCertificateAuthorityId', 'groupCertificateAuthorityProperties_groupCertificateAuthorityId' - The ID of the certificate authority for the group.
newGroupCertificateAuthorityProperties ::
  GroupCertificateAuthorityProperties
newGroupCertificateAuthorityProperties =
  GroupCertificateAuthorityProperties'
    { groupCertificateAuthorityArn =
        Prelude.Nothing,
      groupCertificateAuthorityId =
        Prelude.Nothing
    }

-- | The ARN of the certificate authority for the group.
groupCertificateAuthorityProperties_groupCertificateAuthorityArn :: Lens.Lens' GroupCertificateAuthorityProperties (Prelude.Maybe Prelude.Text)
groupCertificateAuthorityProperties_groupCertificateAuthorityArn = Lens.lens (\GroupCertificateAuthorityProperties' {groupCertificateAuthorityArn} -> groupCertificateAuthorityArn) (\s@GroupCertificateAuthorityProperties' {} a -> s {groupCertificateAuthorityArn = a} :: GroupCertificateAuthorityProperties)

-- | The ID of the certificate authority for the group.
groupCertificateAuthorityProperties_groupCertificateAuthorityId :: Lens.Lens' GroupCertificateAuthorityProperties (Prelude.Maybe Prelude.Text)
groupCertificateAuthorityProperties_groupCertificateAuthorityId = Lens.lens (\GroupCertificateAuthorityProperties' {groupCertificateAuthorityId} -> groupCertificateAuthorityId) (\s@GroupCertificateAuthorityProperties' {} a -> s {groupCertificateAuthorityId = a} :: GroupCertificateAuthorityProperties)

instance
  Data.FromJSON
    GroupCertificateAuthorityProperties
  where
  parseJSON =
    Data.withObject
      "GroupCertificateAuthorityProperties"
      ( \x ->
          GroupCertificateAuthorityProperties'
            Prelude.<$> (x Data..:? "GroupCertificateAuthorityArn")
            Prelude.<*> (x Data..:? "GroupCertificateAuthorityId")
      )

instance
  Prelude.Hashable
    GroupCertificateAuthorityProperties
  where
  hashWithSalt
    _salt
    GroupCertificateAuthorityProperties' {..} =
      _salt
        `Prelude.hashWithSalt` groupCertificateAuthorityArn
        `Prelude.hashWithSalt` groupCertificateAuthorityId

instance
  Prelude.NFData
    GroupCertificateAuthorityProperties
  where
  rnf GroupCertificateAuthorityProperties' {..} =
    Prelude.rnf groupCertificateAuthorityArn
      `Prelude.seq` Prelude.rnf groupCertificateAuthorityId
