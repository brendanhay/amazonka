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
-- Module      : Amazonka.Redshift.AssociateDataShareConsumer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From a datashare consumer account, associates a datashare with the
-- account (AssociateEntireAccount) or the specified namespace
-- (ConsumerArn). If you make this association, the consumer can consume
-- the datashare.
module Amazonka.Redshift.AssociateDataShareConsumer
  ( -- * Creating a Request
    AssociateDataShareConsumer (..),
    newAssociateDataShareConsumer,

    -- * Request Lenses
    associateDataShareConsumer_associateEntireAccount,
    associateDataShareConsumer_consumerArn,
    associateDataShareConsumer_consumerRegion,
    associateDataShareConsumer_dataShareArn,

    -- * Destructuring the Response
    DataShare (..),
    newDataShare,

    -- * Response Lenses
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateDataShareConsumer' smart constructor.
data AssociateDataShareConsumer = AssociateDataShareConsumer'
  { -- | A value that specifies whether the datashare is associated with the
    -- entire account.
    associateEntireAccount :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the consumer that is associated with
    -- the datashare.
    consumerArn :: Prelude.Maybe Prelude.Text,
    -- | From a datashare consumer account, associates a datashare with all
    -- existing and future namespaces in the specified Amazon Web Services
    -- Region.
    consumerRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the datashare that the consumer is to
    -- use with the account or the namespace.
    dataShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDataShareConsumer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associateEntireAccount', 'associateDataShareConsumer_associateEntireAccount' - A value that specifies whether the datashare is associated with the
-- entire account.
--
-- 'consumerArn', 'associateDataShareConsumer_consumerArn' - The Amazon Resource Name (ARN) of the consumer that is associated with
-- the datashare.
--
-- 'consumerRegion', 'associateDataShareConsumer_consumerRegion' - From a datashare consumer account, associates a datashare with all
-- existing and future namespaces in the specified Amazon Web Services
-- Region.
--
-- 'dataShareArn', 'associateDataShareConsumer_dataShareArn' - The Amazon Resource Name (ARN) of the datashare that the consumer is to
-- use with the account or the namespace.
newAssociateDataShareConsumer ::
  -- | 'dataShareArn'
  Prelude.Text ->
  AssociateDataShareConsumer
newAssociateDataShareConsumer pDataShareArn_ =
  AssociateDataShareConsumer'
    { associateEntireAccount =
        Prelude.Nothing,
      consumerArn = Prelude.Nothing,
      consumerRegion = Prelude.Nothing,
      dataShareArn = pDataShareArn_
    }

-- | A value that specifies whether the datashare is associated with the
-- entire account.
associateDataShareConsumer_associateEntireAccount :: Lens.Lens' AssociateDataShareConsumer (Prelude.Maybe Prelude.Bool)
associateDataShareConsumer_associateEntireAccount = Lens.lens (\AssociateDataShareConsumer' {associateEntireAccount} -> associateEntireAccount) (\s@AssociateDataShareConsumer' {} a -> s {associateEntireAccount = a} :: AssociateDataShareConsumer)

-- | The Amazon Resource Name (ARN) of the consumer that is associated with
-- the datashare.
associateDataShareConsumer_consumerArn :: Lens.Lens' AssociateDataShareConsumer (Prelude.Maybe Prelude.Text)
associateDataShareConsumer_consumerArn = Lens.lens (\AssociateDataShareConsumer' {consumerArn} -> consumerArn) (\s@AssociateDataShareConsumer' {} a -> s {consumerArn = a} :: AssociateDataShareConsumer)

-- | From a datashare consumer account, associates a datashare with all
-- existing and future namespaces in the specified Amazon Web Services
-- Region.
associateDataShareConsumer_consumerRegion :: Lens.Lens' AssociateDataShareConsumer (Prelude.Maybe Prelude.Text)
associateDataShareConsumer_consumerRegion = Lens.lens (\AssociateDataShareConsumer' {consumerRegion} -> consumerRegion) (\s@AssociateDataShareConsumer' {} a -> s {consumerRegion = a} :: AssociateDataShareConsumer)

-- | The Amazon Resource Name (ARN) of the datashare that the consumer is to
-- use with the account or the namespace.
associateDataShareConsumer_dataShareArn :: Lens.Lens' AssociateDataShareConsumer Prelude.Text
associateDataShareConsumer_dataShareArn = Lens.lens (\AssociateDataShareConsumer' {dataShareArn} -> dataShareArn) (\s@AssociateDataShareConsumer' {} a -> s {dataShareArn = a} :: AssociateDataShareConsumer)

instance Core.AWSRequest AssociateDataShareConsumer where
  type
    AWSResponse AssociateDataShareConsumer =
      DataShare
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AssociateDataShareConsumerResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable AssociateDataShareConsumer where
  hashWithSalt _salt AssociateDataShareConsumer' {..} =
    _salt `Prelude.hashWithSalt` associateEntireAccount
      `Prelude.hashWithSalt` consumerArn
      `Prelude.hashWithSalt` consumerRegion
      `Prelude.hashWithSalt` dataShareArn

instance Prelude.NFData AssociateDataShareConsumer where
  rnf AssociateDataShareConsumer' {..} =
    Prelude.rnf associateEntireAccount
      `Prelude.seq` Prelude.rnf consumerArn
      `Prelude.seq` Prelude.rnf consumerRegion
      `Prelude.seq` Prelude.rnf dataShareArn

instance Data.ToHeaders AssociateDataShareConsumer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateDataShareConsumer where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateDataShareConsumer where
  toQuery AssociateDataShareConsumer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssociateDataShareConsumer" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "AssociateEntireAccount"
          Data.=: associateEntireAccount,
        "ConsumerArn" Data.=: consumerArn,
        "ConsumerRegion" Data.=: consumerRegion,
        "DataShareArn" Data.=: dataShareArn
      ]
