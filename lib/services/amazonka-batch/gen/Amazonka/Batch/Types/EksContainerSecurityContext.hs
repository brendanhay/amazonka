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
-- Module      : Amazonka.Batch.Types.EksContainerSecurityContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksContainerSecurityContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The security context for a job. For more information, see
-- <https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ Configure a security context for a pod or container>
-- in the /Kubernetes documentation/.
--
-- /See:/ 'newEksContainerSecurityContext' smart constructor.
data EksContainerSecurityContext = EksContainerSecurityContext'
  { -- | When this parameter is @true@, the container is given elevated
    -- permissions on the host container instance. The level of permissions are
    -- similar to the @root@ user permissions. The default value is @false@.
    -- This parameter maps to @privileged@ policy in the
    -- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#privileged Privileged pod security policies>
    -- in the /Kubernetes documentation/.
    privileged :: Prelude.Maybe Prelude.Bool,
    -- | When this parameter is @true@, the container is given read-only access
    -- to its root file system. The default value is @false@. This parameter
    -- maps to @ReadOnlyRootFilesystem@ policy in the
    -- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#volumes-and-file-systems Volumes and file systems pod security policies>
    -- in the /Kubernetes documentation/.
    readOnlyRootFilesystem :: Prelude.Maybe Prelude.Bool,
    -- | When this parameter is specified, the container is run as the specified
    -- group ID (@gid@). If this parameter isn\'t specified, the default is the
    -- group that\'s specified in the image metadata. This parameter maps to
    -- @RunAsGroup@ and @MustRunAs@ policy in the
    -- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
    -- in the /Kubernetes documentation/.
    runAsGroup :: Prelude.Maybe Prelude.Integer,
    -- | When this parameter is specified, the container is run as a user with a
    -- @uid@ other than 0. If this parameter isn\'t specified, so such rule is
    -- enforced. This parameter maps to @RunAsUser@ and @MustRunAsNonRoot@
    -- policy in the
    -- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
    -- in the /Kubernetes documentation/.
    runAsNonRoot :: Prelude.Maybe Prelude.Bool,
    -- | When this parameter is specified, the container is run as the specified
    -- user ID (@uid@). If this parameter isn\'t specified, the default is the
    -- user that\'s specified in the image metadata. This parameter maps to
    -- @RunAsUser@ and @MustRanAs@ policy in the
    -- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
    -- in the /Kubernetes documentation/.
    runAsUser :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksContainerSecurityContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privileged', 'eksContainerSecurityContext_privileged' - When this parameter is @true@, the container is given elevated
-- permissions on the host container instance. The level of permissions are
-- similar to the @root@ user permissions. The default value is @false@.
-- This parameter maps to @privileged@ policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#privileged Privileged pod security policies>
-- in the /Kubernetes documentation/.
--
-- 'readOnlyRootFilesystem', 'eksContainerSecurityContext_readOnlyRootFilesystem' - When this parameter is @true@, the container is given read-only access
-- to its root file system. The default value is @false@. This parameter
-- maps to @ReadOnlyRootFilesystem@ policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#volumes-and-file-systems Volumes and file systems pod security policies>
-- in the /Kubernetes documentation/.
--
-- 'runAsGroup', 'eksContainerSecurityContext_runAsGroup' - When this parameter is specified, the container is run as the specified
-- group ID (@gid@). If this parameter isn\'t specified, the default is the
-- group that\'s specified in the image metadata. This parameter maps to
-- @RunAsGroup@ and @MustRunAs@ policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
-- in the /Kubernetes documentation/.
--
-- 'runAsNonRoot', 'eksContainerSecurityContext_runAsNonRoot' - When this parameter is specified, the container is run as a user with a
-- @uid@ other than 0. If this parameter isn\'t specified, so such rule is
-- enforced. This parameter maps to @RunAsUser@ and @MustRunAsNonRoot@
-- policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
-- in the /Kubernetes documentation/.
--
-- 'runAsUser', 'eksContainerSecurityContext_runAsUser' - When this parameter is specified, the container is run as the specified
-- user ID (@uid@). If this parameter isn\'t specified, the default is the
-- user that\'s specified in the image metadata. This parameter maps to
-- @RunAsUser@ and @MustRanAs@ policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
-- in the /Kubernetes documentation/.
newEksContainerSecurityContext ::
  EksContainerSecurityContext
newEksContainerSecurityContext =
  EksContainerSecurityContext'
    { privileged =
        Prelude.Nothing,
      readOnlyRootFilesystem = Prelude.Nothing,
      runAsGroup = Prelude.Nothing,
      runAsNonRoot = Prelude.Nothing,
      runAsUser = Prelude.Nothing
    }

-- | When this parameter is @true@, the container is given elevated
-- permissions on the host container instance. The level of permissions are
-- similar to the @root@ user permissions. The default value is @false@.
-- This parameter maps to @privileged@ policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#privileged Privileged pod security policies>
-- in the /Kubernetes documentation/.
eksContainerSecurityContext_privileged :: Lens.Lens' EksContainerSecurityContext (Prelude.Maybe Prelude.Bool)
eksContainerSecurityContext_privileged = Lens.lens (\EksContainerSecurityContext' {privileged} -> privileged) (\s@EksContainerSecurityContext' {} a -> s {privileged = a} :: EksContainerSecurityContext)

-- | When this parameter is @true@, the container is given read-only access
-- to its root file system. The default value is @false@. This parameter
-- maps to @ReadOnlyRootFilesystem@ policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#volumes-and-file-systems Volumes and file systems pod security policies>
-- in the /Kubernetes documentation/.
eksContainerSecurityContext_readOnlyRootFilesystem :: Lens.Lens' EksContainerSecurityContext (Prelude.Maybe Prelude.Bool)
eksContainerSecurityContext_readOnlyRootFilesystem = Lens.lens (\EksContainerSecurityContext' {readOnlyRootFilesystem} -> readOnlyRootFilesystem) (\s@EksContainerSecurityContext' {} a -> s {readOnlyRootFilesystem = a} :: EksContainerSecurityContext)

-- | When this parameter is specified, the container is run as the specified
-- group ID (@gid@). If this parameter isn\'t specified, the default is the
-- group that\'s specified in the image metadata. This parameter maps to
-- @RunAsGroup@ and @MustRunAs@ policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
-- in the /Kubernetes documentation/.
eksContainerSecurityContext_runAsGroup :: Lens.Lens' EksContainerSecurityContext (Prelude.Maybe Prelude.Integer)
eksContainerSecurityContext_runAsGroup = Lens.lens (\EksContainerSecurityContext' {runAsGroup} -> runAsGroup) (\s@EksContainerSecurityContext' {} a -> s {runAsGroup = a} :: EksContainerSecurityContext)

-- | When this parameter is specified, the container is run as a user with a
-- @uid@ other than 0. If this parameter isn\'t specified, so such rule is
-- enforced. This parameter maps to @RunAsUser@ and @MustRunAsNonRoot@
-- policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
-- in the /Kubernetes documentation/.
eksContainerSecurityContext_runAsNonRoot :: Lens.Lens' EksContainerSecurityContext (Prelude.Maybe Prelude.Bool)
eksContainerSecurityContext_runAsNonRoot = Lens.lens (\EksContainerSecurityContext' {runAsNonRoot} -> runAsNonRoot) (\s@EksContainerSecurityContext' {} a -> s {runAsNonRoot = a} :: EksContainerSecurityContext)

-- | When this parameter is specified, the container is run as the specified
-- user ID (@uid@). If this parameter isn\'t specified, the default is the
-- user that\'s specified in the image metadata. This parameter maps to
-- @RunAsUser@ and @MustRanAs@ policy in the
-- <https://kubernetes.io/docs/concepts/security/pod-security-policy/#users-and-groups Users and groups pod security policies>
-- in the /Kubernetes documentation/.
eksContainerSecurityContext_runAsUser :: Lens.Lens' EksContainerSecurityContext (Prelude.Maybe Prelude.Integer)
eksContainerSecurityContext_runAsUser = Lens.lens (\EksContainerSecurityContext' {runAsUser} -> runAsUser) (\s@EksContainerSecurityContext' {} a -> s {runAsUser = a} :: EksContainerSecurityContext)

instance Data.FromJSON EksContainerSecurityContext where
  parseJSON =
    Data.withObject
      "EksContainerSecurityContext"
      ( \x ->
          EksContainerSecurityContext'
            Prelude.<$> (x Data..:? "privileged")
            Prelude.<*> (x Data..:? "readOnlyRootFilesystem")
            Prelude.<*> (x Data..:? "runAsGroup")
            Prelude.<*> (x Data..:? "runAsNonRoot")
            Prelude.<*> (x Data..:? "runAsUser")
      )

instance Prelude.Hashable EksContainerSecurityContext where
  hashWithSalt _salt EksContainerSecurityContext' {..} =
    _salt
      `Prelude.hashWithSalt` privileged
      `Prelude.hashWithSalt` readOnlyRootFilesystem
      `Prelude.hashWithSalt` runAsGroup
      `Prelude.hashWithSalt` runAsNonRoot
      `Prelude.hashWithSalt` runAsUser

instance Prelude.NFData EksContainerSecurityContext where
  rnf EksContainerSecurityContext' {..} =
    Prelude.rnf privileged
      `Prelude.seq` Prelude.rnf readOnlyRootFilesystem
      `Prelude.seq` Prelude.rnf runAsGroup
      `Prelude.seq` Prelude.rnf runAsNonRoot
      `Prelude.seq` Prelude.rnf runAsUser

instance Data.ToJSON EksContainerSecurityContext where
  toJSON EksContainerSecurityContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("privileged" Data..=) Prelude.<$> privileged,
            ("readOnlyRootFilesystem" Data..=)
              Prelude.<$> readOnlyRootFilesystem,
            ("runAsGroup" Data..=) Prelude.<$> runAsGroup,
            ("runAsNonRoot" Data..=) Prelude.<$> runAsNonRoot,
            ("runAsUser" Data..=) Prelude.<$> runAsUser
          ]
      )
