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
-- Module      : Amazonka.GuardDuty.Types.KubernetesUserDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesUserDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about the Kubernetes user involved in a Kubernetes finding.
--
-- /See:/ 'newKubernetesUserDetails' smart constructor.
data KubernetesUserDetails = KubernetesUserDetails'
  { -- | The username of the user who called the Kubernetes API.
    username :: Prelude.Maybe Prelude.Text,
    -- | The user ID of the user who called the Kubernetes API.
    uid :: Prelude.Maybe Prelude.Text,
    -- | The groups that include the user who called the Kubernetes API.
    groups :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesUserDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'kubernetesUserDetails_username' - The username of the user who called the Kubernetes API.
--
-- 'uid', 'kubernetesUserDetails_uid' - The user ID of the user who called the Kubernetes API.
--
-- 'groups', 'kubernetesUserDetails_groups' - The groups that include the user who called the Kubernetes API.
newKubernetesUserDetails ::
  KubernetesUserDetails
newKubernetesUserDetails =
  KubernetesUserDetails'
    { username = Prelude.Nothing,
      uid = Prelude.Nothing,
      groups = Prelude.Nothing
    }

-- | The username of the user who called the Kubernetes API.
kubernetesUserDetails_username :: Lens.Lens' KubernetesUserDetails (Prelude.Maybe Prelude.Text)
kubernetesUserDetails_username = Lens.lens (\KubernetesUserDetails' {username} -> username) (\s@KubernetesUserDetails' {} a -> s {username = a} :: KubernetesUserDetails)

-- | The user ID of the user who called the Kubernetes API.
kubernetesUserDetails_uid :: Lens.Lens' KubernetesUserDetails (Prelude.Maybe Prelude.Text)
kubernetesUserDetails_uid = Lens.lens (\KubernetesUserDetails' {uid} -> uid) (\s@KubernetesUserDetails' {} a -> s {uid = a} :: KubernetesUserDetails)

-- | The groups that include the user who called the Kubernetes API.
kubernetesUserDetails_groups :: Lens.Lens' KubernetesUserDetails (Prelude.Maybe [Prelude.Text])
kubernetesUserDetails_groups = Lens.lens (\KubernetesUserDetails' {groups} -> groups) (\s@KubernetesUserDetails' {} a -> s {groups = a} :: KubernetesUserDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON KubernetesUserDetails where
  parseJSON =
    Core.withObject
      "KubernetesUserDetails"
      ( \x ->
          KubernetesUserDetails'
            Prelude.<$> (x Core..:? "username")
            Prelude.<*> (x Core..:? "uid")
            Prelude.<*> (x Core..:? "groups" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable KubernetesUserDetails where
  hashWithSalt _salt KubernetesUserDetails' {..} =
    _salt `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` uid
      `Prelude.hashWithSalt` groups

instance Prelude.NFData KubernetesUserDetails where
  rnf KubernetesUserDetails' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf uid
      `Prelude.seq` Prelude.rnf groups
