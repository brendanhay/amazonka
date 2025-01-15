{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.DescribeAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the association for the specified target or managed node. If
-- you created the association by using the @Targets@ parameter, then you
-- must retrieve the association by using the association ID.
module Amazonka.SSM.DescribeAssociation
  ( -- * Creating a Request
    DescribeAssociation (..),
    newDescribeAssociation,

    -- * Request Lenses
    describeAssociation_associationId,
    describeAssociation_associationVersion,
    describeAssociation_instanceId,
    describeAssociation_name,

    -- * Destructuring the Response
    DescribeAssociationResponse (..),
    newDescribeAssociationResponse,

    -- * Response Lenses
    describeAssociationResponse_associationDescription,
    describeAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeAssociation' smart constructor.
data DescribeAssociation = DescribeAssociation'
  { -- | The association ID for which you want information.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | Specify the association version to retrieve. To view the latest version,
    -- either specify @$LATEST@ for this parameter, or omit this parameter. To
    -- view a list of all associations for a managed node, use
    -- ListAssociations. To get a list of versions for a specific association,
    -- use ListAssociationVersions.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'describeAssociation_associationId' - The association ID for which you want information.
--
-- 'associationVersion', 'describeAssociation_associationVersion' - Specify the association version to retrieve. To view the latest version,
-- either specify @$LATEST@ for this parameter, or omit this parameter. To
-- view a list of all associations for a managed node, use
-- ListAssociations. To get a list of versions for a specific association,
-- use ListAssociationVersions.
--
-- 'instanceId', 'describeAssociation_instanceId' - The managed node ID.
--
-- 'name', 'describeAssociation_name' - The name of the SSM document.
newDescribeAssociation ::
  DescribeAssociation
newDescribeAssociation =
  DescribeAssociation'
    { associationId =
        Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The association ID for which you want information.
describeAssociation_associationId :: Lens.Lens' DescribeAssociation (Prelude.Maybe Prelude.Text)
describeAssociation_associationId = Lens.lens (\DescribeAssociation' {associationId} -> associationId) (\s@DescribeAssociation' {} a -> s {associationId = a} :: DescribeAssociation)

-- | Specify the association version to retrieve. To view the latest version,
-- either specify @$LATEST@ for this parameter, or omit this parameter. To
-- view a list of all associations for a managed node, use
-- ListAssociations. To get a list of versions for a specific association,
-- use ListAssociationVersions.
describeAssociation_associationVersion :: Lens.Lens' DescribeAssociation (Prelude.Maybe Prelude.Text)
describeAssociation_associationVersion = Lens.lens (\DescribeAssociation' {associationVersion} -> associationVersion) (\s@DescribeAssociation' {} a -> s {associationVersion = a} :: DescribeAssociation)

-- | The managed node ID.
describeAssociation_instanceId :: Lens.Lens' DescribeAssociation (Prelude.Maybe Prelude.Text)
describeAssociation_instanceId = Lens.lens (\DescribeAssociation' {instanceId} -> instanceId) (\s@DescribeAssociation' {} a -> s {instanceId = a} :: DescribeAssociation)

-- | The name of the SSM document.
describeAssociation_name :: Lens.Lens' DescribeAssociation (Prelude.Maybe Prelude.Text)
describeAssociation_name = Lens.lens (\DescribeAssociation' {name} -> name) (\s@DescribeAssociation' {} a -> s {name = a} :: DescribeAssociation)

instance Core.AWSRequest DescribeAssociation where
  type
    AWSResponse DescribeAssociation =
      DescribeAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssociationResponse'
            Prelude.<$> (x Data..?> "AssociationDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAssociation where
  hashWithSalt _salt DescribeAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeAssociation where
  rnf DescribeAssociation' {..} =
    Prelude.rnf associationId `Prelude.seq`
      Prelude.rnf associationVersion `Prelude.seq`
        Prelude.rnf instanceId `Prelude.seq`
          Prelude.rnf name

instance Data.ToHeaders DescribeAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAssociation where
  toJSON DescribeAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationId" Data..=) Prelude.<$> associationId,
            ("AssociationVersion" Data..=)
              Prelude.<$> associationVersion,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath DescribeAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssociationResponse' smart constructor.
data DescribeAssociationResponse = DescribeAssociationResponse'
  { -- | Information about the association.
    associationDescription :: Prelude.Maybe AssociationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationDescription', 'describeAssociationResponse_associationDescription' - Information about the association.
--
-- 'httpStatus', 'describeAssociationResponse_httpStatus' - The response's http status code.
newDescribeAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAssociationResponse
newDescribeAssociationResponse pHttpStatus_ =
  DescribeAssociationResponse'
    { associationDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the association.
describeAssociationResponse_associationDescription :: Lens.Lens' DescribeAssociationResponse (Prelude.Maybe AssociationDescription)
describeAssociationResponse_associationDescription = Lens.lens (\DescribeAssociationResponse' {associationDescription} -> associationDescription) (\s@DescribeAssociationResponse' {} a -> s {associationDescription = a} :: DescribeAssociationResponse)

-- | The response's http status code.
describeAssociationResponse_httpStatus :: Lens.Lens' DescribeAssociationResponse Prelude.Int
describeAssociationResponse_httpStatus = Lens.lens (\DescribeAssociationResponse' {httpStatus} -> httpStatus) (\s@DescribeAssociationResponse' {} a -> s {httpStatus = a} :: DescribeAssociationResponse)

instance Prelude.NFData DescribeAssociationResponse where
  rnf DescribeAssociationResponse' {..} =
    Prelude.rnf associationDescription `Prelude.seq`
      Prelude.rnf httpStatus
