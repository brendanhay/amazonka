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
-- Module      : Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociationError
import Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociationStatus

-- | In the response to an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverQueryLogConfig.html AssociateResolverQueryLogConfig>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_DisassociateResolverQueryLogConfig.html DisassociateResolverQueryLogConfig>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverQueryLogConfigAssociation.html GetResolverQueryLogConfigAssociation>,
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_ListResolverQueryLogConfigAssociations.html ListResolverQueryLogConfigAssociations>,
-- request, a complex type that contains settings for a specified
-- association between an Amazon VPC and a query logging configuration.
--
-- /See:/ 'newResolverQueryLogConfigAssociation' smart constructor.
data ResolverQueryLogConfigAssociation = ResolverQueryLogConfigAssociation'
  { -- | The date and time that the VPC was associated with the query logging
    -- configuration, in Unix time format and Coordinated Universal Time (UTC).
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | If the value of @Status@ is @FAILED@, the value of @Error@ indicates the
    -- cause:
    --
    -- -   @DESTINATION_NOT_FOUND@: The specified destination (for example, an
    --     Amazon S3 bucket) was deleted.
    --
    -- -   @ACCESS_DENIED@: Permissions don\'t allow sending logs to the
    --     destination.
    --
    -- If the value of @Status@ is a value other than @FAILED@, @Error@ is
    -- null.
    error :: Prelude.Maybe ResolverQueryLogConfigAssociationError,
    -- | Contains additional information about the error. If the value or @Error@
    -- is null, the value of @ErrorMessage@ also is null.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the query logging association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the query logging configuration that a VPC is associated with.
    resolverQueryLogConfigId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon VPC that is associated with the query logging
    -- configuration.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The status of the specified query logging association. Valid values
    -- include the following:
    --
    -- -   @CREATING@: Resolver is creating an association between an Amazon
    --     VPC and a query logging configuration.
    --
    -- -   @CREATED@: The association between an Amazon VPC and a query logging
    --     configuration was successfully created. Resolver is logging queries
    --     that originate in the specified VPC.
    --
    -- -   @DELETING@: Resolver is deleting this query logging association.
    --
    -- -   @FAILED@: Resolver either couldn\'t create or couldn\'t delete the
    --     query logging association.
    status :: Prelude.Maybe ResolverQueryLogConfigAssociationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolverQueryLogConfigAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'resolverQueryLogConfigAssociation_creationTime' - The date and time that the VPC was associated with the query logging
-- configuration, in Unix time format and Coordinated Universal Time (UTC).
--
-- 'error', 'resolverQueryLogConfigAssociation_error' - If the value of @Status@ is @FAILED@, the value of @Error@ indicates the
-- cause:
--
-- -   @DESTINATION_NOT_FOUND@: The specified destination (for example, an
--     Amazon S3 bucket) was deleted.
--
-- -   @ACCESS_DENIED@: Permissions don\'t allow sending logs to the
--     destination.
--
-- If the value of @Status@ is a value other than @FAILED@, @Error@ is
-- null.
--
-- 'errorMessage', 'resolverQueryLogConfigAssociation_errorMessage' - Contains additional information about the error. If the value or @Error@
-- is null, the value of @ErrorMessage@ also is null.
--
-- 'id', 'resolverQueryLogConfigAssociation_id' - The ID of the query logging association.
--
-- 'resolverQueryLogConfigId', 'resolverQueryLogConfigAssociation_resolverQueryLogConfigId' - The ID of the query logging configuration that a VPC is associated with.
--
-- 'resourceId', 'resolverQueryLogConfigAssociation_resourceId' - The ID of the Amazon VPC that is associated with the query logging
-- configuration.
--
-- 'status', 'resolverQueryLogConfigAssociation_status' - The status of the specified query logging association. Valid values
-- include the following:
--
-- -   @CREATING@: Resolver is creating an association between an Amazon
--     VPC and a query logging configuration.
--
-- -   @CREATED@: The association between an Amazon VPC and a query logging
--     configuration was successfully created. Resolver is logging queries
--     that originate in the specified VPC.
--
-- -   @DELETING@: Resolver is deleting this query logging association.
--
-- -   @FAILED@: Resolver either couldn\'t create or couldn\'t delete the
--     query logging association.
newResolverQueryLogConfigAssociation ::
  ResolverQueryLogConfigAssociation
newResolverQueryLogConfigAssociation =
  ResolverQueryLogConfigAssociation'
    { creationTime =
        Prelude.Nothing,
      error = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      resolverQueryLogConfigId =
        Prelude.Nothing,
      resourceId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time that the VPC was associated with the query logging
-- configuration, in Unix time format and Coordinated Universal Time (UTC).
resolverQueryLogConfigAssociation_creationTime :: Lens.Lens' ResolverQueryLogConfigAssociation (Prelude.Maybe Prelude.Text)
resolverQueryLogConfigAssociation_creationTime = Lens.lens (\ResolverQueryLogConfigAssociation' {creationTime} -> creationTime) (\s@ResolverQueryLogConfigAssociation' {} a -> s {creationTime = a} :: ResolverQueryLogConfigAssociation)

-- | If the value of @Status@ is @FAILED@, the value of @Error@ indicates the
-- cause:
--
-- -   @DESTINATION_NOT_FOUND@: The specified destination (for example, an
--     Amazon S3 bucket) was deleted.
--
-- -   @ACCESS_DENIED@: Permissions don\'t allow sending logs to the
--     destination.
--
-- If the value of @Status@ is a value other than @FAILED@, @Error@ is
-- null.
resolverQueryLogConfigAssociation_error :: Lens.Lens' ResolverQueryLogConfigAssociation (Prelude.Maybe ResolverQueryLogConfigAssociationError)
resolverQueryLogConfigAssociation_error = Lens.lens (\ResolverQueryLogConfigAssociation' {error} -> error) (\s@ResolverQueryLogConfigAssociation' {} a -> s {error = a} :: ResolverQueryLogConfigAssociation)

-- | Contains additional information about the error. If the value or @Error@
-- is null, the value of @ErrorMessage@ also is null.
resolverQueryLogConfigAssociation_errorMessage :: Lens.Lens' ResolverQueryLogConfigAssociation (Prelude.Maybe Prelude.Text)
resolverQueryLogConfigAssociation_errorMessage = Lens.lens (\ResolverQueryLogConfigAssociation' {errorMessage} -> errorMessage) (\s@ResolverQueryLogConfigAssociation' {} a -> s {errorMessage = a} :: ResolverQueryLogConfigAssociation)

-- | The ID of the query logging association.
resolverQueryLogConfigAssociation_id :: Lens.Lens' ResolverQueryLogConfigAssociation (Prelude.Maybe Prelude.Text)
resolverQueryLogConfigAssociation_id = Lens.lens (\ResolverQueryLogConfigAssociation' {id} -> id) (\s@ResolverQueryLogConfigAssociation' {} a -> s {id = a} :: ResolverQueryLogConfigAssociation)

-- | The ID of the query logging configuration that a VPC is associated with.
resolverQueryLogConfigAssociation_resolverQueryLogConfigId :: Lens.Lens' ResolverQueryLogConfigAssociation (Prelude.Maybe Prelude.Text)
resolverQueryLogConfigAssociation_resolverQueryLogConfigId = Lens.lens (\ResolverQueryLogConfigAssociation' {resolverQueryLogConfigId} -> resolverQueryLogConfigId) (\s@ResolverQueryLogConfigAssociation' {} a -> s {resolverQueryLogConfigId = a} :: ResolverQueryLogConfigAssociation)

-- | The ID of the Amazon VPC that is associated with the query logging
-- configuration.
resolverQueryLogConfigAssociation_resourceId :: Lens.Lens' ResolverQueryLogConfigAssociation (Prelude.Maybe Prelude.Text)
resolverQueryLogConfigAssociation_resourceId = Lens.lens (\ResolverQueryLogConfigAssociation' {resourceId} -> resourceId) (\s@ResolverQueryLogConfigAssociation' {} a -> s {resourceId = a} :: ResolverQueryLogConfigAssociation)

-- | The status of the specified query logging association. Valid values
-- include the following:
--
-- -   @CREATING@: Resolver is creating an association between an Amazon
--     VPC and a query logging configuration.
--
-- -   @CREATED@: The association between an Amazon VPC and a query logging
--     configuration was successfully created. Resolver is logging queries
--     that originate in the specified VPC.
--
-- -   @DELETING@: Resolver is deleting this query logging association.
--
-- -   @FAILED@: Resolver either couldn\'t create or couldn\'t delete the
--     query logging association.
resolverQueryLogConfigAssociation_status :: Lens.Lens' ResolverQueryLogConfigAssociation (Prelude.Maybe ResolverQueryLogConfigAssociationStatus)
resolverQueryLogConfigAssociation_status = Lens.lens (\ResolverQueryLogConfigAssociation' {status} -> status) (\s@ResolverQueryLogConfigAssociation' {} a -> s {status = a} :: ResolverQueryLogConfigAssociation)

instance
  Data.FromJSON
    ResolverQueryLogConfigAssociation
  where
  parseJSON =
    Data.withObject
      "ResolverQueryLogConfigAssociation"
      ( \x ->
          ResolverQueryLogConfigAssociation'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "ResolverQueryLogConfigId")
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    ResolverQueryLogConfigAssociation
  where
  hashWithSalt
    _salt
    ResolverQueryLogConfigAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` error
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` resolverQueryLogConfigId
        `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    ResolverQueryLogConfigAssociation
  where
  rnf ResolverQueryLogConfigAssociation' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resolverQueryLogConfigId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf status
