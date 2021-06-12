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
-- Module      : Network.AWS.SSM.Types.Activation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Activation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.Tag

-- | An activation registers one or more on-premises servers or virtual
-- machines (VMs) with AWS so that you can configure those servers or VMs
-- using Run Command. A server or VM that has been registered with AWS is
-- called a managed instance.
--
-- /See:/ 'newActivation' smart constructor.
data Activation = Activation'
  { -- | The maximum number of managed instances that can be registered using
    -- this activation.
    registrationLimit :: Core.Maybe Core.Natural,
    -- | The date the activation was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The ID created by Systems Manager when you submitted the activation.
    activationId :: Core.Maybe Core.Text,
    -- | The Amazon Identity and Access Management (IAM) role to assign to the
    -- managed instance.
    iamRole :: Core.Maybe Core.Text,
    -- | A name for the managed instance when it is created.
    defaultInstanceName :: Core.Maybe Core.Text,
    -- | The date when this activation can no longer be used to register managed
    -- instances.
    expirationDate :: Core.Maybe Core.POSIX,
    -- | Whether or not the activation is expired.
    expired :: Core.Maybe Core.Bool,
    -- | The number of managed instances already registered with this activation.
    registrationsCount :: Core.Maybe Core.Natural,
    -- | Tags assigned to the activation.
    tags :: Core.Maybe [Tag],
    -- | A user defined description of the activation.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Activation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registrationLimit', 'activation_registrationLimit' - The maximum number of managed instances that can be registered using
-- this activation.
--
-- 'createdDate', 'activation_createdDate' - The date the activation was created.
--
-- 'activationId', 'activation_activationId' - The ID created by Systems Manager when you submitted the activation.
--
-- 'iamRole', 'activation_iamRole' - The Amazon Identity and Access Management (IAM) role to assign to the
-- managed instance.
--
-- 'defaultInstanceName', 'activation_defaultInstanceName' - A name for the managed instance when it is created.
--
-- 'expirationDate', 'activation_expirationDate' - The date when this activation can no longer be used to register managed
-- instances.
--
-- 'expired', 'activation_expired' - Whether or not the activation is expired.
--
-- 'registrationsCount', 'activation_registrationsCount' - The number of managed instances already registered with this activation.
--
-- 'tags', 'activation_tags' - Tags assigned to the activation.
--
-- 'description', 'activation_description' - A user defined description of the activation.
newActivation ::
  Activation
newActivation =
  Activation'
    { registrationLimit = Core.Nothing,
      createdDate = Core.Nothing,
      activationId = Core.Nothing,
      iamRole = Core.Nothing,
      defaultInstanceName = Core.Nothing,
      expirationDate = Core.Nothing,
      expired = Core.Nothing,
      registrationsCount = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing
    }

-- | The maximum number of managed instances that can be registered using
-- this activation.
activation_registrationLimit :: Lens.Lens' Activation (Core.Maybe Core.Natural)
activation_registrationLimit = Lens.lens (\Activation' {registrationLimit} -> registrationLimit) (\s@Activation' {} a -> s {registrationLimit = a} :: Activation)

-- | The date the activation was created.
activation_createdDate :: Lens.Lens' Activation (Core.Maybe Core.UTCTime)
activation_createdDate = Lens.lens (\Activation' {createdDate} -> createdDate) (\s@Activation' {} a -> s {createdDate = a} :: Activation) Core.. Lens.mapping Core._Time

-- | The ID created by Systems Manager when you submitted the activation.
activation_activationId :: Lens.Lens' Activation (Core.Maybe Core.Text)
activation_activationId = Lens.lens (\Activation' {activationId} -> activationId) (\s@Activation' {} a -> s {activationId = a} :: Activation)

-- | The Amazon Identity and Access Management (IAM) role to assign to the
-- managed instance.
activation_iamRole :: Lens.Lens' Activation (Core.Maybe Core.Text)
activation_iamRole = Lens.lens (\Activation' {iamRole} -> iamRole) (\s@Activation' {} a -> s {iamRole = a} :: Activation)

-- | A name for the managed instance when it is created.
activation_defaultInstanceName :: Lens.Lens' Activation (Core.Maybe Core.Text)
activation_defaultInstanceName = Lens.lens (\Activation' {defaultInstanceName} -> defaultInstanceName) (\s@Activation' {} a -> s {defaultInstanceName = a} :: Activation)

-- | The date when this activation can no longer be used to register managed
-- instances.
activation_expirationDate :: Lens.Lens' Activation (Core.Maybe Core.UTCTime)
activation_expirationDate = Lens.lens (\Activation' {expirationDate} -> expirationDate) (\s@Activation' {} a -> s {expirationDate = a} :: Activation) Core.. Lens.mapping Core._Time

-- | Whether or not the activation is expired.
activation_expired :: Lens.Lens' Activation (Core.Maybe Core.Bool)
activation_expired = Lens.lens (\Activation' {expired} -> expired) (\s@Activation' {} a -> s {expired = a} :: Activation)

-- | The number of managed instances already registered with this activation.
activation_registrationsCount :: Lens.Lens' Activation (Core.Maybe Core.Natural)
activation_registrationsCount = Lens.lens (\Activation' {registrationsCount} -> registrationsCount) (\s@Activation' {} a -> s {registrationsCount = a} :: Activation)

-- | Tags assigned to the activation.
activation_tags :: Lens.Lens' Activation (Core.Maybe [Tag])
activation_tags = Lens.lens (\Activation' {tags} -> tags) (\s@Activation' {} a -> s {tags = a} :: Activation) Core.. Lens.mapping Lens._Coerce

-- | A user defined description of the activation.
activation_description :: Lens.Lens' Activation (Core.Maybe Core.Text)
activation_description = Lens.lens (\Activation' {description} -> description) (\s@Activation' {} a -> s {description = a} :: Activation)

instance Core.FromJSON Activation where
  parseJSON =
    Core.withObject
      "Activation"
      ( \x ->
          Activation'
            Core.<$> (x Core..:? "RegistrationLimit")
            Core.<*> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "ActivationId")
            Core.<*> (x Core..:? "IamRole")
            Core.<*> (x Core..:? "DefaultInstanceName")
            Core.<*> (x Core..:? "ExpirationDate")
            Core.<*> (x Core..:? "Expired")
            Core.<*> (x Core..:? "RegistrationsCount")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable Activation

instance Core.NFData Activation
