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
-- Module      : Amazonka.EC2.DescribeKeyPairs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified key pairs or all of your key pairs.
--
-- For more information about key pairs, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 key pairs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.DescribeKeyPairs
  ( -- * Creating a Request
    DescribeKeyPairs (..),
    newDescribeKeyPairs,

    -- * Request Lenses
    describeKeyPairs_dryRun,
    describeKeyPairs_filters,
    describeKeyPairs_includePublicKey,
    describeKeyPairs_keyNames,
    describeKeyPairs_keyPairIds,

    -- * Destructuring the Response
    DescribeKeyPairsResponse (..),
    newDescribeKeyPairsResponse,

    -- * Response Lenses
    describeKeyPairsResponse_keyPairs,
    describeKeyPairsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeKeyPairs' smart constructor.
data DescribeKeyPairs = DescribeKeyPairs'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @key-pair-id@ - The ID of the key pair.
    --
    -- -   @fingerprint@ - The fingerprint of the key pair.
    --
    -- -   @key-name@ - The name of the key pair.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    filters :: Prelude.Maybe [Filter],
    -- | If @true@, the public key material is included in the response.
    --
    -- Default: @false@
    includePublicKey :: Prelude.Maybe Prelude.Bool,
    -- | The key pair names.
    --
    -- Default: Describes all of your key pairs.
    keyNames :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the key pairs.
    keyPairIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeKeyPairs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeKeyPairs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeKeyPairs_filters' - The filters.
--
-- -   @key-pair-id@ - The ID of the key pair.
--
-- -   @fingerprint@ - The fingerprint of the key pair.
--
-- -   @key-name@ - The name of the key pair.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- 'includePublicKey', 'describeKeyPairs_includePublicKey' - If @true@, the public key material is included in the response.
--
-- Default: @false@
--
-- 'keyNames', 'describeKeyPairs_keyNames' - The key pair names.
--
-- Default: Describes all of your key pairs.
--
-- 'keyPairIds', 'describeKeyPairs_keyPairIds' - The IDs of the key pairs.
newDescribeKeyPairs ::
  DescribeKeyPairs
newDescribeKeyPairs =
  DescribeKeyPairs'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      includePublicKey = Prelude.Nothing,
      keyNames = Prelude.Nothing,
      keyPairIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeKeyPairs_dryRun :: Lens.Lens' DescribeKeyPairs (Prelude.Maybe Prelude.Bool)
describeKeyPairs_dryRun = Lens.lens (\DescribeKeyPairs' {dryRun} -> dryRun) (\s@DescribeKeyPairs' {} a -> s {dryRun = a} :: DescribeKeyPairs)

-- | The filters.
--
-- -   @key-pair-id@ - The ID of the key pair.
--
-- -   @fingerprint@ - The fingerprint of the key pair.
--
-- -   @key-name@ - The name of the key pair.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
describeKeyPairs_filters :: Lens.Lens' DescribeKeyPairs (Prelude.Maybe [Filter])
describeKeyPairs_filters = Lens.lens (\DescribeKeyPairs' {filters} -> filters) (\s@DescribeKeyPairs' {} a -> s {filters = a} :: DescribeKeyPairs) Prelude.. Lens.mapping Lens.coerced

-- | If @true@, the public key material is included in the response.
--
-- Default: @false@
describeKeyPairs_includePublicKey :: Lens.Lens' DescribeKeyPairs (Prelude.Maybe Prelude.Bool)
describeKeyPairs_includePublicKey = Lens.lens (\DescribeKeyPairs' {includePublicKey} -> includePublicKey) (\s@DescribeKeyPairs' {} a -> s {includePublicKey = a} :: DescribeKeyPairs)

-- | The key pair names.
--
-- Default: Describes all of your key pairs.
describeKeyPairs_keyNames :: Lens.Lens' DescribeKeyPairs (Prelude.Maybe [Prelude.Text])
describeKeyPairs_keyNames = Lens.lens (\DescribeKeyPairs' {keyNames} -> keyNames) (\s@DescribeKeyPairs' {} a -> s {keyNames = a} :: DescribeKeyPairs) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the key pairs.
describeKeyPairs_keyPairIds :: Lens.Lens' DescribeKeyPairs (Prelude.Maybe [Prelude.Text])
describeKeyPairs_keyPairIds = Lens.lens (\DescribeKeyPairs' {keyPairIds} -> keyPairIds) (\s@DescribeKeyPairs' {} a -> s {keyPairIds = a} :: DescribeKeyPairs) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeKeyPairs where
  type
    AWSResponse DescribeKeyPairs =
      DescribeKeyPairsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeKeyPairsResponse'
            Prelude.<$> ( x
                            Data..@? "keySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeKeyPairs where
  hashWithSalt _salt DescribeKeyPairs' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` includePublicKey
      `Prelude.hashWithSalt` keyNames
      `Prelude.hashWithSalt` keyPairIds

instance Prelude.NFData DescribeKeyPairs where
  rnf DescribeKeyPairs' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includePublicKey
      `Prelude.seq` Prelude.rnf keyNames
      `Prelude.seq` Prelude.rnf keyPairIds

instance Data.ToHeaders DescribeKeyPairs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeKeyPairs where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeKeyPairs where
  toQuery DescribeKeyPairs' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeKeyPairs" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "IncludePublicKey" Data.=: includePublicKey,
        Data.toQuery
          (Data.toQueryList "KeyName" Prelude.<$> keyNames),
        Data.toQuery
          ( Data.toQueryList "KeyPairId"
              Prelude.<$> keyPairIds
          )
      ]

-- | /See:/ 'newDescribeKeyPairsResponse' smart constructor.
data DescribeKeyPairsResponse = DescribeKeyPairsResponse'
  { -- | Information about the key pairs.
    keyPairs :: Prelude.Maybe [KeyPairInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeKeyPairsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairs', 'describeKeyPairsResponse_keyPairs' - Information about the key pairs.
--
-- 'httpStatus', 'describeKeyPairsResponse_httpStatus' - The response's http status code.
newDescribeKeyPairsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeKeyPairsResponse
newDescribeKeyPairsResponse pHttpStatus_ =
  DescribeKeyPairsResponse'
    { keyPairs =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the key pairs.
describeKeyPairsResponse_keyPairs :: Lens.Lens' DescribeKeyPairsResponse (Prelude.Maybe [KeyPairInfo])
describeKeyPairsResponse_keyPairs = Lens.lens (\DescribeKeyPairsResponse' {keyPairs} -> keyPairs) (\s@DescribeKeyPairsResponse' {} a -> s {keyPairs = a} :: DescribeKeyPairsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeKeyPairsResponse_httpStatus :: Lens.Lens' DescribeKeyPairsResponse Prelude.Int
describeKeyPairsResponse_httpStatus = Lens.lens (\DescribeKeyPairsResponse' {httpStatus} -> httpStatus) (\s@DescribeKeyPairsResponse' {} a -> s {httpStatus = a} :: DescribeKeyPairsResponse)

instance Prelude.NFData DescribeKeyPairsResponse where
  rnf DescribeKeyPairsResponse' {..} =
    Prelude.rnf keyPairs
      `Prelude.seq` Prelude.rnf httpStatus
