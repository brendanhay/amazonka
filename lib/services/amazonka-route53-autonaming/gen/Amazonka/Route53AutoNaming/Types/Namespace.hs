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
-- Module      : Amazonka.Route53AutoNaming.Types.Namespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.Namespace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.NamespaceProperties
import Amazonka.Route53AutoNaming.Types.NamespaceType

-- | A complex type that contains information about a specified namespace.
--
-- /See:/ 'newNamespace' smart constructor.
data Namespace = Namespace'
  { -- | The Amazon Resource Name (ARN) that Cloud Map assigns to the namespace
    -- when you create it.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date that the namespace was created, in Unix date\/time format and
    -- Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate
    -- to milliseconds. For example, the value @1516925490.087@ represents
    -- Friday, January 26, 2018 12:11:30.087 AM.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | A unique string that identifies the request and that allows failed
    -- requests to be retried without the risk of running an operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The description that you specify for the namespace when you create it.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of a namespace.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the namespace, such as @example.com@.
    name :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information that\'s specific to the type of
    -- the namespace.
    properties :: Prelude.Maybe NamespaceProperties,
    -- | The number of services that are associated with the namespace.
    serviceCount :: Prelude.Maybe Prelude.Int,
    -- | The type of the namespace. The methods for discovering instances depends
    -- on the value that you specify:
    --
    -- [HTTP]
    --     Instances can be discovered only programmatically, using the Cloud
    --     Map @DiscoverInstances@ API.
    --
    -- [DNS_PUBLIC]
    --     Instances can be discovered using public DNS queries and using the
    --     @DiscoverInstances@ API.
    --
    -- [DNS_PRIVATE]
    --     Instances can be discovered using DNS queries in VPCs and using the
    --     @DiscoverInstances@ API.
    type' :: Prelude.Maybe NamespaceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Namespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'namespace_arn' - The Amazon Resource Name (ARN) that Cloud Map assigns to the namespace
-- when you create it.
--
-- 'createDate', 'namespace_createDate' - The date that the namespace was created, in Unix date\/time format and
-- Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate
-- to milliseconds. For example, the value @1516925490.087@ represents
-- Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'creatorRequestId', 'namespace_creatorRequestId' - A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running an operation twice.
--
-- 'description', 'namespace_description' - The description that you specify for the namespace when you create it.
--
-- 'id', 'namespace_id' - The ID of a namespace.
--
-- 'name', 'namespace_name' - The name of the namespace, such as @example.com@.
--
-- 'properties', 'namespace_properties' - A complex type that contains information that\'s specific to the type of
-- the namespace.
--
-- 'serviceCount', 'namespace_serviceCount' - The number of services that are associated with the namespace.
--
-- 'type'', 'namespace_type' - The type of the namespace. The methods for discovering instances depends
-- on the value that you specify:
--
-- [HTTP]
--     Instances can be discovered only programmatically, using the Cloud
--     Map @DiscoverInstances@ API.
--
-- [DNS_PUBLIC]
--     Instances can be discovered using public DNS queries and using the
--     @DiscoverInstances@ API.
--
-- [DNS_PRIVATE]
--     Instances can be discovered using DNS queries in VPCs and using the
--     @DiscoverInstances@ API.
newNamespace ::
  Namespace
newNamespace =
  Namespace'
    { arn = Prelude.Nothing,
      createDate = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      properties = Prelude.Nothing,
      serviceCount = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that Cloud Map assigns to the namespace
-- when you create it.
namespace_arn :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_arn = Lens.lens (\Namespace' {arn} -> arn) (\s@Namespace' {} a -> s {arn = a} :: Namespace)

-- | The date that the namespace was created, in Unix date\/time format and
-- Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate
-- to milliseconds. For example, the value @1516925490.087@ represents
-- Friday, January 26, 2018 12:11:30.087 AM.
namespace_createDate :: Lens.Lens' Namespace (Prelude.Maybe Prelude.UTCTime)
namespace_createDate = Lens.lens (\Namespace' {createDate} -> createDate) (\s@Namespace' {} a -> s {createDate = a} :: Namespace) Prelude.. Lens.mapping Data._Time

-- | A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of running an operation twice.
namespace_creatorRequestId :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_creatorRequestId = Lens.lens (\Namespace' {creatorRequestId} -> creatorRequestId) (\s@Namespace' {} a -> s {creatorRequestId = a} :: Namespace)

-- | The description that you specify for the namespace when you create it.
namespace_description :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_description = Lens.lens (\Namespace' {description} -> description) (\s@Namespace' {} a -> s {description = a} :: Namespace)

-- | The ID of a namespace.
namespace_id :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_id = Lens.lens (\Namespace' {id} -> id) (\s@Namespace' {} a -> s {id = a} :: Namespace)

-- | The name of the namespace, such as @example.com@.
namespace_name :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_name = Lens.lens (\Namespace' {name} -> name) (\s@Namespace' {} a -> s {name = a} :: Namespace)

-- | A complex type that contains information that\'s specific to the type of
-- the namespace.
namespace_properties :: Lens.Lens' Namespace (Prelude.Maybe NamespaceProperties)
namespace_properties = Lens.lens (\Namespace' {properties} -> properties) (\s@Namespace' {} a -> s {properties = a} :: Namespace)

-- | The number of services that are associated with the namespace.
namespace_serviceCount :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Int)
namespace_serviceCount = Lens.lens (\Namespace' {serviceCount} -> serviceCount) (\s@Namespace' {} a -> s {serviceCount = a} :: Namespace)

-- | The type of the namespace. The methods for discovering instances depends
-- on the value that you specify:
--
-- [HTTP]
--     Instances can be discovered only programmatically, using the Cloud
--     Map @DiscoverInstances@ API.
--
-- [DNS_PUBLIC]
--     Instances can be discovered using public DNS queries and using the
--     @DiscoverInstances@ API.
--
-- [DNS_PRIVATE]
--     Instances can be discovered using DNS queries in VPCs and using the
--     @DiscoverInstances@ API.
namespace_type :: Lens.Lens' Namespace (Prelude.Maybe NamespaceType)
namespace_type = Lens.lens (\Namespace' {type'} -> type') (\s@Namespace' {} a -> s {type' = a} :: Namespace)

instance Data.FromJSON Namespace where
  parseJSON =
    Data.withObject
      "Namespace"
      ( \x ->
          Namespace'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "CreatorRequestId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Properties")
            Prelude.<*> (x Data..:? "ServiceCount")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Namespace where
  hashWithSalt _salt Namespace' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` serviceCount
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Namespace where
  rnf Namespace' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf serviceCount
      `Prelude.seq` Prelude.rnf type'
