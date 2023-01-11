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
-- Module      : Amazonka.EKS.DescribeAddon
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Amazon EKS add-on.
module Amazonka.EKS.DescribeAddon
  ( -- * Creating a Request
    DescribeAddon (..),
    newDescribeAddon,

    -- * Request Lenses
    describeAddon_clusterName,
    describeAddon_addonName,

    -- * Destructuring the Response
    DescribeAddonResponse (..),
    newDescribeAddonResponse,

    -- * Response Lenses
    describeAddonResponse_addon,
    describeAddonResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAddon' smart constructor.
data DescribeAddon = DescribeAddon'
  { -- | The name of the cluster.
    clusterName :: Prelude.Text,
    -- | The name of the add-on. The name must match one of the names returned by
    -- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
    -- .
    addonName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'describeAddon_clusterName' - The name of the cluster.
--
-- 'addonName', 'describeAddon_addonName' - The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
newDescribeAddon ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'addonName'
  Prelude.Text ->
  DescribeAddon
newDescribeAddon pClusterName_ pAddonName_ =
  DescribeAddon'
    { clusterName = pClusterName_,
      addonName = pAddonName_
    }

-- | The name of the cluster.
describeAddon_clusterName :: Lens.Lens' DescribeAddon Prelude.Text
describeAddon_clusterName = Lens.lens (\DescribeAddon' {clusterName} -> clusterName) (\s@DescribeAddon' {} a -> s {clusterName = a} :: DescribeAddon)

-- | The name of the add-on. The name must match one of the names returned by
-- <https://docs.aws.amazon.com/eks/latest/APIReference/API_ListAddons.html ListAddons>
-- .
describeAddon_addonName :: Lens.Lens' DescribeAddon Prelude.Text
describeAddon_addonName = Lens.lens (\DescribeAddon' {addonName} -> addonName) (\s@DescribeAddon' {} a -> s {addonName = a} :: DescribeAddon)

instance Core.AWSRequest DescribeAddon where
  type
    AWSResponse DescribeAddon =
      DescribeAddonResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddonResponse'
            Prelude.<$> (x Data..?> "addon")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddon where
  hashWithSalt _salt DescribeAddon' {..} =
    _salt `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` addonName

instance Prelude.NFData DescribeAddon where
  rnf DescribeAddon' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf addonName

instance Data.ToHeaders DescribeAddon where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAddon where
  toPath DescribeAddon' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Data.toBS clusterName,
        "/addons/",
        Data.toBS addonName
      ]

instance Data.ToQuery DescribeAddon where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAddonResponse' smart constructor.
data DescribeAddonResponse = DescribeAddonResponse'
  { addon :: Prelude.Maybe Addon,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addon', 'describeAddonResponse_addon' - Undocumented member.
--
-- 'httpStatus', 'describeAddonResponse_httpStatus' - The response's http status code.
newDescribeAddonResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAddonResponse
newDescribeAddonResponse pHttpStatus_ =
  DescribeAddonResponse'
    { addon = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeAddonResponse_addon :: Lens.Lens' DescribeAddonResponse (Prelude.Maybe Addon)
describeAddonResponse_addon = Lens.lens (\DescribeAddonResponse' {addon} -> addon) (\s@DescribeAddonResponse' {} a -> s {addon = a} :: DescribeAddonResponse)

-- | The response's http status code.
describeAddonResponse_httpStatus :: Lens.Lens' DescribeAddonResponse Prelude.Int
describeAddonResponse_httpStatus = Lens.lens (\DescribeAddonResponse' {httpStatus} -> httpStatus) (\s@DescribeAddonResponse' {} a -> s {httpStatus = a} :: DescribeAddonResponse)

instance Prelude.NFData DescribeAddonResponse where
  rnf DescribeAddonResponse' {..} =
    Prelude.rnf addon
      `Prelude.seq` Prelude.rnf httpStatus
