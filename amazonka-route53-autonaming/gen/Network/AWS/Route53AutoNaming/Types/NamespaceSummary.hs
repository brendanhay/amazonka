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
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceType

-- | A complex type that contains information about a namespace.
--
-- /See:/ 'newNamespaceSummary' smart constructor.
data NamespaceSummary = NamespaceSummary'
  { -- | The date and time that the namespace was created.
    createDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the
    -- namespace when you create it.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the namespace.
    id :: Core.Maybe Core.Text,
    -- | The name of the namespace. When you create a namespace, AWS Cloud Map
    -- automatically creates a Route 53 hosted zone that has the same name as
    -- the namespace.
    name :: Core.Maybe Core.Text,
    properties :: Core.Maybe NamespaceProperties,
    -- | The number of services that were created using the namespace.
    serviceCount :: Core.Maybe Core.Int,
    -- | A description for the namespace.
    description :: Core.Maybe Core.Text,
    -- | The type of the namespace, either public or private.
    type' :: Core.Maybe NamespaceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NamespaceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDate', 'namespaceSummary_createDate' - The date and time that the namespace was created.
--
-- 'arn', 'namespaceSummary_arn' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the
-- namespace when you create it.
--
-- 'id', 'namespaceSummary_id' - The ID of the namespace.
--
-- 'name', 'namespaceSummary_name' - The name of the namespace. When you create a namespace, AWS Cloud Map
-- automatically creates a Route 53 hosted zone that has the same name as
-- the namespace.
--
-- 'properties', 'namespaceSummary_properties' - Undocumented member.
--
-- 'serviceCount', 'namespaceSummary_serviceCount' - The number of services that were created using the namespace.
--
-- 'description', 'namespaceSummary_description' - A description for the namespace.
--
-- 'type'', 'namespaceSummary_type' - The type of the namespace, either public or private.
newNamespaceSummary ::
  NamespaceSummary
newNamespaceSummary =
  NamespaceSummary'
    { createDate = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      properties = Core.Nothing,
      serviceCount = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing
    }

-- | The date and time that the namespace was created.
namespaceSummary_createDate :: Lens.Lens' NamespaceSummary (Core.Maybe Core.UTCTime)
namespaceSummary_createDate = Lens.lens (\NamespaceSummary' {createDate} -> createDate) (\s@NamespaceSummary' {} a -> s {createDate = a} :: NamespaceSummary) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the
-- namespace when you create it.
namespaceSummary_arn :: Lens.Lens' NamespaceSummary (Core.Maybe Core.Text)
namespaceSummary_arn = Lens.lens (\NamespaceSummary' {arn} -> arn) (\s@NamespaceSummary' {} a -> s {arn = a} :: NamespaceSummary)

-- | The ID of the namespace.
namespaceSummary_id :: Lens.Lens' NamespaceSummary (Core.Maybe Core.Text)
namespaceSummary_id = Lens.lens (\NamespaceSummary' {id} -> id) (\s@NamespaceSummary' {} a -> s {id = a} :: NamespaceSummary)

-- | The name of the namespace. When you create a namespace, AWS Cloud Map
-- automatically creates a Route 53 hosted zone that has the same name as
-- the namespace.
namespaceSummary_name :: Lens.Lens' NamespaceSummary (Core.Maybe Core.Text)
namespaceSummary_name = Lens.lens (\NamespaceSummary' {name} -> name) (\s@NamespaceSummary' {} a -> s {name = a} :: NamespaceSummary)

-- | Undocumented member.
namespaceSummary_properties :: Lens.Lens' NamespaceSummary (Core.Maybe NamespaceProperties)
namespaceSummary_properties = Lens.lens (\NamespaceSummary' {properties} -> properties) (\s@NamespaceSummary' {} a -> s {properties = a} :: NamespaceSummary)

-- | The number of services that were created using the namespace.
namespaceSummary_serviceCount :: Lens.Lens' NamespaceSummary (Core.Maybe Core.Int)
namespaceSummary_serviceCount = Lens.lens (\NamespaceSummary' {serviceCount} -> serviceCount) (\s@NamespaceSummary' {} a -> s {serviceCount = a} :: NamespaceSummary)

-- | A description for the namespace.
namespaceSummary_description :: Lens.Lens' NamespaceSummary (Core.Maybe Core.Text)
namespaceSummary_description = Lens.lens (\NamespaceSummary' {description} -> description) (\s@NamespaceSummary' {} a -> s {description = a} :: NamespaceSummary)

-- | The type of the namespace, either public or private.
namespaceSummary_type :: Lens.Lens' NamespaceSummary (Core.Maybe NamespaceType)
namespaceSummary_type = Lens.lens (\NamespaceSummary' {type'} -> type') (\s@NamespaceSummary' {} a -> s {type' = a} :: NamespaceSummary)

instance Core.FromJSON NamespaceSummary where
  parseJSON =
    Core.withObject
      "NamespaceSummary"
      ( \x ->
          NamespaceSummary'
            Core.<$> (x Core..:? "CreateDate")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Properties")
            Core.<*> (x Core..:? "ServiceCount")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable NamespaceSummary

instance Core.NFData NamespaceSummary
