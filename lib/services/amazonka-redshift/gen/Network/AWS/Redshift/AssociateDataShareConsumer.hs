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
-- Module      : Network.AWS.Redshift.AssociateDataShareConsumer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From a datashare consumer account, associates a datashare with the
-- account (AssociateEntireAccount) or the specified namespace
-- (ConsumerArn). If you make this association, the consumer can consume
-- the datashare.
module Network.AWS.Redshift.AssociateDataShareConsumer
  ( -- * Creating a Request
    AssociateDataShareConsumer (..),
    newAssociateDataShareConsumer,

    -- * Request Lenses
    associateDataShareConsumer_associateEntireAccount,
    associateDataShareConsumer_consumerArn,
    associateDataShareConsumer_dataShareArn,

    -- * Destructuring the Response
    DataShare (..),
    newDataShare,

    -- * Response Lenses
    dataShare_producerArn,
    dataShare_dataShareAssociations,
    dataShare_dataShareArn,
    dataShare_allowPubliclyAccessibleConsumers,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateDataShareConsumer' smart constructor.
data AssociateDataShareConsumer = AssociateDataShareConsumer'
  { -- | A value that specifies whether the datashare is associated with the
    -- entire account.
    associateEntireAccount :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the consumer that is associated with
    -- the datashare.
    consumerArn :: Prelude.Maybe Prelude.Text,
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

-- | The Amazon Resource Name (ARN) of the datashare that the consumer is to
-- use with the account or the namespace.
associateDataShareConsumer_dataShareArn :: Lens.Lens' AssociateDataShareConsumer Prelude.Text
associateDataShareConsumer_dataShareArn = Lens.lens (\AssociateDataShareConsumer' {dataShareArn} -> dataShareArn) (\s@AssociateDataShareConsumer' {} a -> s {dataShareArn = a} :: AssociateDataShareConsumer)

instance Core.AWSRequest AssociateDataShareConsumer where
  type
    AWSResponse AssociateDataShareConsumer =
      DataShare
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AssociateDataShareConsumerResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable AssociateDataShareConsumer

instance Prelude.NFData AssociateDataShareConsumer

instance Core.ToHeaders AssociateDataShareConsumer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AssociateDataShareConsumer where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateDataShareConsumer where
  toQuery AssociateDataShareConsumer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AssociateDataShareConsumer" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "AssociateEntireAccount"
          Core.=: associateEntireAccount,
        "ConsumerArn" Core.=: consumerArn,
        "DataShareArn" Core.=: dataShareArn
      ]
