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
-- Module      : Amazonka.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a network interface attribute. You can specify only one
-- attribute at a time.
module Amazonka.EC2.DescribeNetworkInterfaceAttribute
  ( -- * Creating a Request
    DescribeNetworkInterfaceAttribute (..),
    newDescribeNetworkInterfaceAttribute,

    -- * Request Lenses
    describeNetworkInterfaceAttribute_attribute,
    describeNetworkInterfaceAttribute_dryRun,
    describeNetworkInterfaceAttribute_networkInterfaceId,

    -- * Destructuring the Response
    DescribeNetworkInterfaceAttributeResponse (..),
    newDescribeNetworkInterfaceAttributeResponse,

    -- * Response Lenses
    describeNetworkInterfaceAttributeResponse_attachment,
    describeNetworkInterfaceAttributeResponse_description,
    describeNetworkInterfaceAttributeResponse_groups,
    describeNetworkInterfaceAttributeResponse_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_sourceDestCheck,
    describeNetworkInterfaceAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeNetworkInterfaceAttribute.
--
-- /See:/ 'newDescribeNetworkInterfaceAttribute' smart constructor.
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
  { -- | The attribute of the network interface. This parameter is required.
    attribute :: Prelude.Maybe NetworkInterfaceAttribute,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfaceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'describeNetworkInterfaceAttribute_attribute' - The attribute of the network interface. This parameter is required.
--
-- 'dryRun', 'describeNetworkInterfaceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInterfaceId', 'describeNetworkInterfaceAttribute_networkInterfaceId' - The ID of the network interface.
newDescribeNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  DescribeNetworkInterfaceAttribute
newDescribeNetworkInterfaceAttribute
  pNetworkInterfaceId_ =
    DescribeNetworkInterfaceAttribute'
      { attribute =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        networkInterfaceId =
          pNetworkInterfaceId_
      }

-- | The attribute of the network interface. This parameter is required.
describeNetworkInterfaceAttribute_attribute :: Lens.Lens' DescribeNetworkInterfaceAttribute (Prelude.Maybe NetworkInterfaceAttribute)
describeNetworkInterfaceAttribute_attribute = Lens.lens (\DescribeNetworkInterfaceAttribute' {attribute} -> attribute) (\s@DescribeNetworkInterfaceAttribute' {} a -> s {attribute = a} :: DescribeNetworkInterfaceAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInterfaceAttribute_dryRun :: Lens.Lens' DescribeNetworkInterfaceAttribute (Prelude.Maybe Prelude.Bool)
describeNetworkInterfaceAttribute_dryRun = Lens.lens (\DescribeNetworkInterfaceAttribute' {dryRun} -> dryRun) (\s@DescribeNetworkInterfaceAttribute' {} a -> s {dryRun = a} :: DescribeNetworkInterfaceAttribute)

-- | The ID of the network interface.
describeNetworkInterfaceAttribute_networkInterfaceId :: Lens.Lens' DescribeNetworkInterfaceAttribute Prelude.Text
describeNetworkInterfaceAttribute_networkInterfaceId = Lens.lens (\DescribeNetworkInterfaceAttribute' {networkInterfaceId} -> networkInterfaceId) (\s@DescribeNetworkInterfaceAttribute' {} a -> s {networkInterfaceId = a} :: DescribeNetworkInterfaceAttribute)

instance
  Core.AWSRequest
    DescribeNetworkInterfaceAttribute
  where
  type
    AWSResponse DescribeNetworkInterfaceAttribute =
      DescribeNetworkInterfaceAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInterfaceAttributeResponse'
            Prelude.<$> (x Data..@? "attachment")
              Prelude.<*> (x Data..@? "description")
              Prelude.<*> ( x Data..@? "groupSet" Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Data.parseXMLList "item")
                          )
              Prelude.<*> (x Data..@? "networkInterfaceId")
              Prelude.<*> (x Data..@? "sourceDestCheck")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNetworkInterfaceAttribute
  where
  hashWithSalt
    _salt
    DescribeNetworkInterfaceAttribute' {..} =
      _salt `Prelude.hashWithSalt` attribute
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` networkInterfaceId

instance
  Prelude.NFData
    DescribeNetworkInterfaceAttribute
  where
  rnf DescribeNetworkInterfaceAttribute' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance
  Data.ToHeaders
    DescribeNetworkInterfaceAttribute
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeNetworkInterfaceAttribute
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeNetworkInterfaceAttribute
  where
  toQuery DescribeNetworkInterfaceAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeNetworkInterfaceAttribute" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Attribute" Data.=: attribute,
        "DryRun" Data.=: dryRun,
        "NetworkInterfaceId" Data.=: networkInterfaceId
      ]

-- | Contains the output of DescribeNetworkInterfaceAttribute.
--
-- /See:/ 'newDescribeNetworkInterfaceAttributeResponse' smart constructor.
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
  { -- | The attachment (if any) of the network interface.
    attachment :: Prelude.Maybe NetworkInterfaceAttachment,
    -- | The description of the network interface.
    description :: Prelude.Maybe AttributeValue,
    -- | The security groups associated with the network interface.
    groups :: Prelude.Maybe [GroupIdentifier],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether source\/destination checking is enabled.
    sourceDestCheck :: Prelude.Maybe AttributeBooleanValue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfaceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'describeNetworkInterfaceAttributeResponse_attachment' - The attachment (if any) of the network interface.
--
-- 'description', 'describeNetworkInterfaceAttributeResponse_description' - The description of the network interface.
--
-- 'groups', 'describeNetworkInterfaceAttributeResponse_groups' - The security groups associated with the network interface.
--
-- 'networkInterfaceId', 'describeNetworkInterfaceAttributeResponse_networkInterfaceId' - The ID of the network interface.
--
-- 'sourceDestCheck', 'describeNetworkInterfaceAttributeResponse_sourceDestCheck' - Indicates whether source\/destination checking is enabled.
--
-- 'httpStatus', 'describeNetworkInterfaceAttributeResponse_httpStatus' - The response's http status code.
newDescribeNetworkInterfaceAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInterfaceAttributeResponse
newDescribeNetworkInterfaceAttributeResponse
  pHttpStatus_ =
    DescribeNetworkInterfaceAttributeResponse'
      { attachment =
          Prelude.Nothing,
        description = Prelude.Nothing,
        groups = Prelude.Nothing,
        networkInterfaceId =
          Prelude.Nothing,
        sourceDestCheck =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The attachment (if any) of the network interface.
describeNetworkInterfaceAttributeResponse_attachment :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Prelude.Maybe NetworkInterfaceAttachment)
describeNetworkInterfaceAttributeResponse_attachment = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {attachment} -> attachment) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {attachment = a} :: DescribeNetworkInterfaceAttributeResponse)

-- | The description of the network interface.
describeNetworkInterfaceAttributeResponse_description :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Prelude.Maybe AttributeValue)
describeNetworkInterfaceAttributeResponse_description = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {description} -> description) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {description = a} :: DescribeNetworkInterfaceAttributeResponse)

-- | The security groups associated with the network interface.
describeNetworkInterfaceAttributeResponse_groups :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Prelude.Maybe [GroupIdentifier])
describeNetworkInterfaceAttributeResponse_groups = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {groups} -> groups) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {groups = a} :: DescribeNetworkInterfaceAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
describeNetworkInterfaceAttributeResponse_networkInterfaceId :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Prelude.Maybe Prelude.Text)
describeNetworkInterfaceAttributeResponse_networkInterfaceId = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {networkInterfaceId} -> networkInterfaceId) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {networkInterfaceId = a} :: DescribeNetworkInterfaceAttributeResponse)

-- | Indicates whether source\/destination checking is enabled.
describeNetworkInterfaceAttributeResponse_sourceDestCheck :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Prelude.Maybe AttributeBooleanValue)
describeNetworkInterfaceAttributeResponse_sourceDestCheck = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {sourceDestCheck} -> sourceDestCheck) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {sourceDestCheck = a} :: DescribeNetworkInterfaceAttributeResponse)

-- | The response's http status code.
describeNetworkInterfaceAttributeResponse_httpStatus :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse Prelude.Int
describeNetworkInterfaceAttributeResponse_httpStatus = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInterfaceAttributeResponse)

instance
  Prelude.NFData
    DescribeNetworkInterfaceAttributeResponse
  where
  rnf DescribeNetworkInterfaceAttributeResponse' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf sourceDestCheck
      `Prelude.seq` Prelude.rnf httpStatus
