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
-- Module      : Network.AWS.Route53AutoNaming.Types.Namespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.Namespace where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceType

-- | A complex type that contains information about a specified namespace.
--
-- /See:/ 'newNamespace' smart constructor.
data Namespace = Namespace'
  { -- | The date that the namespace was created, in Unix date\/time format and
    -- Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate
    -- to milliseconds. For example, the value @1516925490.087@ represents
    -- Friday, January 26, 2018 12:11:30.087 AM.
    createDate :: Prelude.Maybe Prelude.POSIX,
    -- | A unique string that identifies the request and that allows failed
    -- requests to be retried without the risk of executing an operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the
    -- namespace when you create it.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a namespace.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the namespace, such as @example.com@.
    name :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information that\'s specific to the type of
    -- the namespace.
    properties :: Prelude.Maybe NamespaceProperties,
    -- | The number of services that are associated with the namespace.
    serviceCount :: Prelude.Maybe Prelude.Int,
    -- | The description that you specify for the namespace when you create it.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the namespace. The methods for discovering instances depends
    -- on the value that you specify:
    --
    -- -   @HTTP@: Instances can be discovered only programmatically, using the
    --     AWS Cloud Map @DiscoverInstances@ API.
    --
    -- -   @DNS_PUBLIC@: Instances can be discovered using public DNS queries
    --     and using the @DiscoverInstances@ API.
    --
    -- -   @DNS_PRIVATE@: Instances can be discovered using DNS queries in VPCs
    --     and using the @DiscoverInstances@ API.
    type' :: Prelude.Maybe NamespaceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Namespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDate', 'namespace_createDate' - The date that the namespace was created, in Unix date\/time format and
-- Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate
-- to milliseconds. For example, the value @1516925490.087@ represents
-- Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'creatorRequestId', 'namespace_creatorRequestId' - A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of executing an operation twice.
--
-- 'arn', 'namespace_arn' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the
-- namespace when you create it.
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
-- 'description', 'namespace_description' - The description that you specify for the namespace when you create it.
--
-- 'type'', 'namespace_type' - The type of the namespace. The methods for discovering instances depends
-- on the value that you specify:
--
-- -   @HTTP@: Instances can be discovered only programmatically, using the
--     AWS Cloud Map @DiscoverInstances@ API.
--
-- -   @DNS_PUBLIC@: Instances can be discovered using public DNS queries
--     and using the @DiscoverInstances@ API.
--
-- -   @DNS_PRIVATE@: Instances can be discovered using DNS queries in VPCs
--     and using the @DiscoverInstances@ API.
newNamespace ::
  Namespace
newNamespace =
  Namespace'
    { createDate = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      properties = Prelude.Nothing,
      serviceCount = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date that the namespace was created, in Unix date\/time format and
-- Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate
-- to milliseconds. For example, the value @1516925490.087@ represents
-- Friday, January 26, 2018 12:11:30.087 AM.
namespace_createDate :: Lens.Lens' Namespace (Prelude.Maybe Prelude.UTCTime)
namespace_createDate = Lens.lens (\Namespace' {createDate} -> createDate) (\s@Namespace' {} a -> s {createDate = a} :: Namespace) Prelude.. Lens.mapping Prelude._Time

-- | A unique string that identifies the request and that allows failed
-- requests to be retried without the risk of executing an operation twice.
namespace_creatorRequestId :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_creatorRequestId = Lens.lens (\Namespace' {creatorRequestId} -> creatorRequestId) (\s@Namespace' {} a -> s {creatorRequestId = a} :: Namespace)

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the
-- namespace when you create it.
namespace_arn :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_arn = Lens.lens (\Namespace' {arn} -> arn) (\s@Namespace' {} a -> s {arn = a} :: Namespace)

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

-- | The description that you specify for the namespace when you create it.
namespace_description :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_description = Lens.lens (\Namespace' {description} -> description) (\s@Namespace' {} a -> s {description = a} :: Namespace)

-- | The type of the namespace. The methods for discovering instances depends
-- on the value that you specify:
--
-- -   @HTTP@: Instances can be discovered only programmatically, using the
--     AWS Cloud Map @DiscoverInstances@ API.
--
-- -   @DNS_PUBLIC@: Instances can be discovered using public DNS queries
--     and using the @DiscoverInstances@ API.
--
-- -   @DNS_PRIVATE@: Instances can be discovered using DNS queries in VPCs
--     and using the @DiscoverInstances@ API.
namespace_type :: Lens.Lens' Namespace (Prelude.Maybe NamespaceType)
namespace_type = Lens.lens (\Namespace' {type'} -> type') (\s@Namespace' {} a -> s {type' = a} :: Namespace)

instance Prelude.FromJSON Namespace where
  parseJSON =
    Prelude.withObject
      "Namespace"
      ( \x ->
          Namespace'
            Prelude.<$> (x Prelude..:? "CreateDate")
            Prelude.<*> (x Prelude..:? "CreatorRequestId")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Properties")
            Prelude.<*> (x Prelude..:? "ServiceCount")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Namespace

instance Prelude.NFData Namespace
