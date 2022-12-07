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
-- Module      : Amazonka.AppStream.Types.Entitlement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.Entitlement where

import Amazonka.AppStream.Types.AppVisibility
import Amazonka.AppStream.Types.EntitlementAttribute
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an entitlement. Entitlements control access to specific
-- applications within a stack, based on user attributes. Entitlements
-- apply to SAML 2.0 federated user identities. Amazon AppStream 2.0 user
-- pool and streaming URL users are entitled to all applications in a
-- stack. Entitlements don\'t apply to the desktop stream view application,
-- or to applications managed by a dynamic app provider using the Dynamic
-- Application Framework.
--
-- /See:/ 'newEntitlement' smart constructor.
data Entitlement = Entitlement'
  { -- | The time when the entitlement was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the entitlement.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time when the entitlement was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the entitlement.
    name :: Prelude.Text,
    -- | The name of the stack with which the entitlement is associated.
    stackName :: Prelude.Text,
    -- | Specifies whether all or selected apps are entitled.
    appVisibility :: AppVisibility,
    -- | The attributes of the entitlement.
    attributes :: Prelude.NonEmpty EntitlementAttribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Entitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'entitlement_createdTime' - The time when the entitlement was created.
--
-- 'description', 'entitlement_description' - The description of the entitlement.
--
-- 'lastModifiedTime', 'entitlement_lastModifiedTime' - The time when the entitlement was last modified.
--
-- 'name', 'entitlement_name' - The name of the entitlement.
--
-- 'stackName', 'entitlement_stackName' - The name of the stack with which the entitlement is associated.
--
-- 'appVisibility', 'entitlement_appVisibility' - Specifies whether all or selected apps are entitled.
--
-- 'attributes', 'entitlement_attributes' - The attributes of the entitlement.
newEntitlement ::
  -- | 'name'
  Prelude.Text ->
  -- | 'stackName'
  Prelude.Text ->
  -- | 'appVisibility'
  AppVisibility ->
  -- | 'attributes'
  Prelude.NonEmpty EntitlementAttribute ->
  Entitlement
newEntitlement
  pName_
  pStackName_
  pAppVisibility_
  pAttributes_ =
    Entitlement'
      { createdTime = Prelude.Nothing,
        description = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        name = pName_,
        stackName = pStackName_,
        appVisibility = pAppVisibility_,
        attributes = Lens.coerced Lens.# pAttributes_
      }

-- | The time when the entitlement was created.
entitlement_createdTime :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.UTCTime)
entitlement_createdTime = Lens.lens (\Entitlement' {createdTime} -> createdTime) (\s@Entitlement' {} a -> s {createdTime = a} :: Entitlement) Prelude.. Lens.mapping Data._Time

-- | The description of the entitlement.
entitlement_description :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Text)
entitlement_description = Lens.lens (\Entitlement' {description} -> description) (\s@Entitlement' {} a -> s {description = a} :: Entitlement)

-- | The time when the entitlement was last modified.
entitlement_lastModifiedTime :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.UTCTime)
entitlement_lastModifiedTime = Lens.lens (\Entitlement' {lastModifiedTime} -> lastModifiedTime) (\s@Entitlement' {} a -> s {lastModifiedTime = a} :: Entitlement) Prelude.. Lens.mapping Data._Time

-- | The name of the entitlement.
entitlement_name :: Lens.Lens' Entitlement Prelude.Text
entitlement_name = Lens.lens (\Entitlement' {name} -> name) (\s@Entitlement' {} a -> s {name = a} :: Entitlement)

-- | The name of the stack with which the entitlement is associated.
entitlement_stackName :: Lens.Lens' Entitlement Prelude.Text
entitlement_stackName = Lens.lens (\Entitlement' {stackName} -> stackName) (\s@Entitlement' {} a -> s {stackName = a} :: Entitlement)

-- | Specifies whether all or selected apps are entitled.
entitlement_appVisibility :: Lens.Lens' Entitlement AppVisibility
entitlement_appVisibility = Lens.lens (\Entitlement' {appVisibility} -> appVisibility) (\s@Entitlement' {} a -> s {appVisibility = a} :: Entitlement)

-- | The attributes of the entitlement.
entitlement_attributes :: Lens.Lens' Entitlement (Prelude.NonEmpty EntitlementAttribute)
entitlement_attributes = Lens.lens (\Entitlement' {attributes} -> attributes) (\s@Entitlement' {} a -> s {attributes = a} :: Entitlement) Prelude.. Lens.coerced

instance Data.FromJSON Entitlement where
  parseJSON =
    Data.withObject
      "Entitlement"
      ( \x ->
          Entitlement'
            Prelude.<$> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "StackName")
            Prelude.<*> (x Data..: "AppVisibility")
            Prelude.<*> (x Data..: "Attributes")
      )

instance Prelude.Hashable Entitlement where
  hashWithSalt _salt Entitlement' {..} =
    _salt `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` appVisibility
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData Entitlement where
  rnf Entitlement' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf appVisibility
      `Prelude.seq` Prelude.rnf attributes
