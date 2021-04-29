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
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceType

-- | A complex type that contains information about a namespace.
--
-- /See:/ 'newNamespaceSummary' smart constructor.
data NamespaceSummary = NamespaceSummary'
  { -- | The date and time that the namespace was created.
    createDate :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the
    -- namespace when you create it.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the namespace.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the namespace. When you create a namespace, AWS Cloud Map
    -- automatically creates a Route 53 hosted zone that has the same name as
    -- the namespace.
    name :: Prelude.Maybe Prelude.Text,
    properties :: Prelude.Maybe NamespaceProperties,
    -- | The number of services that were created using the namespace.
    serviceCount :: Prelude.Maybe Prelude.Int,
    -- | A description for the namespace.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the namespace, either public or private.
    type' :: Prelude.Maybe NamespaceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { createDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      properties = Prelude.Nothing,
      serviceCount = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date and time that the namespace was created.
namespaceSummary_createDate :: Lens.Lens' NamespaceSummary (Prelude.Maybe Prelude.UTCTime)
namespaceSummary_createDate = Lens.lens (\NamespaceSummary' {createDate} -> createDate) (\s@NamespaceSummary' {} a -> s {createDate = a} :: NamespaceSummary) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the
-- namespace when you create it.
namespaceSummary_arn :: Lens.Lens' NamespaceSummary (Prelude.Maybe Prelude.Text)
namespaceSummary_arn = Lens.lens (\NamespaceSummary' {arn} -> arn) (\s@NamespaceSummary' {} a -> s {arn = a} :: NamespaceSummary)

-- | The ID of the namespace.
namespaceSummary_id :: Lens.Lens' NamespaceSummary (Prelude.Maybe Prelude.Text)
namespaceSummary_id = Lens.lens (\NamespaceSummary' {id} -> id) (\s@NamespaceSummary' {} a -> s {id = a} :: NamespaceSummary)

-- | The name of the namespace. When you create a namespace, AWS Cloud Map
-- automatically creates a Route 53 hosted zone that has the same name as
-- the namespace.
namespaceSummary_name :: Lens.Lens' NamespaceSummary (Prelude.Maybe Prelude.Text)
namespaceSummary_name = Lens.lens (\NamespaceSummary' {name} -> name) (\s@NamespaceSummary' {} a -> s {name = a} :: NamespaceSummary)

-- | Undocumented member.
namespaceSummary_properties :: Lens.Lens' NamespaceSummary (Prelude.Maybe NamespaceProperties)
namespaceSummary_properties = Lens.lens (\NamespaceSummary' {properties} -> properties) (\s@NamespaceSummary' {} a -> s {properties = a} :: NamespaceSummary)

-- | The number of services that were created using the namespace.
namespaceSummary_serviceCount :: Lens.Lens' NamespaceSummary (Prelude.Maybe Prelude.Int)
namespaceSummary_serviceCount = Lens.lens (\NamespaceSummary' {serviceCount} -> serviceCount) (\s@NamespaceSummary' {} a -> s {serviceCount = a} :: NamespaceSummary)

-- | A description for the namespace.
namespaceSummary_description :: Lens.Lens' NamespaceSummary (Prelude.Maybe Prelude.Text)
namespaceSummary_description = Lens.lens (\NamespaceSummary' {description} -> description) (\s@NamespaceSummary' {} a -> s {description = a} :: NamespaceSummary)

-- | The type of the namespace, either public or private.
namespaceSummary_type :: Lens.Lens' NamespaceSummary (Prelude.Maybe NamespaceType)
namespaceSummary_type = Lens.lens (\NamespaceSummary' {type'} -> type') (\s@NamespaceSummary' {} a -> s {type' = a} :: NamespaceSummary)

instance Prelude.FromJSON NamespaceSummary where
  parseJSON =
    Prelude.withObject
      "NamespaceSummary"
      ( \x ->
          NamespaceSummary'
            Prelude.<$> (x Prelude..:? "CreateDate")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Properties")
            Prelude.<*> (x Prelude..:? "ServiceCount")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable NamespaceSummary

instance Prelude.NFData NamespaceSummary
