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
-- Module      : Amazonka.Redshift.DisassociateDataShareConsumer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From a datashare consumer account, remove association for the specified
-- datashare.
module Amazonka.Redshift.DisassociateDataShareConsumer
  ( -- * Creating a Request
    DisassociateDataShareConsumer (..),
    newDisassociateDataShareConsumer,

    -- * Request Lenses
    disassociateDataShareConsumer_consumerArn,
    disassociateDataShareConsumer_consumerRegion,
    disassociateDataShareConsumer_disassociateEntireAccount,
    disassociateDataShareConsumer_dataShareArn,

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

-- | /See:/ 'newDisassociateDataShareConsumer' smart constructor.
data DisassociateDataShareConsumer = DisassociateDataShareConsumer'
  { -- | The Amazon Resource Name (ARN) of the consumer that association for the
    -- datashare is removed from.
    consumerArn :: Prelude.Maybe Prelude.Text,
    -- | From a datashare consumer account, removes association of a datashare
    -- from all the existing and future namespaces in the specified Amazon Web
    -- Services Region.
    consumerRegion :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies whether association for the datashare is removed
    -- from the entire account.
    disassociateEntireAccount :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the datashare to remove association
    -- for.
    dataShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDataShareConsumer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumerArn', 'disassociateDataShareConsumer_consumerArn' - The Amazon Resource Name (ARN) of the consumer that association for the
-- datashare is removed from.
--
-- 'consumerRegion', 'disassociateDataShareConsumer_consumerRegion' - From a datashare consumer account, removes association of a datashare
-- from all the existing and future namespaces in the specified Amazon Web
-- Services Region.
--
-- 'disassociateEntireAccount', 'disassociateDataShareConsumer_disassociateEntireAccount' - A value that specifies whether association for the datashare is removed
-- from the entire account.
--
-- 'dataShareArn', 'disassociateDataShareConsumer_dataShareArn' - The Amazon Resource Name (ARN) of the datashare to remove association
-- for.
newDisassociateDataShareConsumer ::
  -- | 'dataShareArn'
  Prelude.Text ->
  DisassociateDataShareConsumer
newDisassociateDataShareConsumer pDataShareArn_ =
  DisassociateDataShareConsumer'
    { consumerArn =
        Prelude.Nothing,
      consumerRegion = Prelude.Nothing,
      disassociateEntireAccount = Prelude.Nothing,
      dataShareArn = pDataShareArn_
    }

-- | The Amazon Resource Name (ARN) of the consumer that association for the
-- datashare is removed from.
disassociateDataShareConsumer_consumerArn :: Lens.Lens' DisassociateDataShareConsumer (Prelude.Maybe Prelude.Text)
disassociateDataShareConsumer_consumerArn = Lens.lens (\DisassociateDataShareConsumer' {consumerArn} -> consumerArn) (\s@DisassociateDataShareConsumer' {} a -> s {consumerArn = a} :: DisassociateDataShareConsumer)

-- | From a datashare consumer account, removes association of a datashare
-- from all the existing and future namespaces in the specified Amazon Web
-- Services Region.
disassociateDataShareConsumer_consumerRegion :: Lens.Lens' DisassociateDataShareConsumer (Prelude.Maybe Prelude.Text)
disassociateDataShareConsumer_consumerRegion = Lens.lens (\DisassociateDataShareConsumer' {consumerRegion} -> consumerRegion) (\s@DisassociateDataShareConsumer' {} a -> s {consumerRegion = a} :: DisassociateDataShareConsumer)

-- | A value that specifies whether association for the datashare is removed
-- from the entire account.
disassociateDataShareConsumer_disassociateEntireAccount :: Lens.Lens' DisassociateDataShareConsumer (Prelude.Maybe Prelude.Bool)
disassociateDataShareConsumer_disassociateEntireAccount = Lens.lens (\DisassociateDataShareConsumer' {disassociateEntireAccount} -> disassociateEntireAccount) (\s@DisassociateDataShareConsumer' {} a -> s {disassociateEntireAccount = a} :: DisassociateDataShareConsumer)

-- | The Amazon Resource Name (ARN) of the datashare to remove association
-- for.
disassociateDataShareConsumer_dataShareArn :: Lens.Lens' DisassociateDataShareConsumer Prelude.Text
disassociateDataShareConsumer_dataShareArn = Lens.lens (\DisassociateDataShareConsumer' {dataShareArn} -> dataShareArn) (\s@DisassociateDataShareConsumer' {} a -> s {dataShareArn = a} :: DisassociateDataShareConsumer)

instance
  Core.AWSRequest
    DisassociateDataShareConsumer
  where
  type
    AWSResponse DisassociateDataShareConsumer =
      DataShare
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DisassociateDataShareConsumerResult"
      (\s h x -> Data.parseXML x)

instance
  Prelude.Hashable
    DisassociateDataShareConsumer
  where
  hashWithSalt _salt DisassociateDataShareConsumer' {..} =
    _salt
      `Prelude.hashWithSalt` consumerArn
      `Prelude.hashWithSalt` consumerRegion
      `Prelude.hashWithSalt` disassociateEntireAccount
      `Prelude.hashWithSalt` dataShareArn

instance Prelude.NFData DisassociateDataShareConsumer where
  rnf DisassociateDataShareConsumer' {..} =
    Prelude.rnf consumerArn
      `Prelude.seq` Prelude.rnf consumerRegion
      `Prelude.seq` Prelude.rnf disassociateEntireAccount
      `Prelude.seq` Prelude.rnf dataShareArn

instance Data.ToHeaders DisassociateDataShareConsumer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisassociateDataShareConsumer where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateDataShareConsumer where
  toQuery DisassociateDataShareConsumer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DisassociateDataShareConsumer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ConsumerArn" Data.=: consumerArn,
        "ConsumerRegion" Data.=: consumerRegion,
        "DisassociateEntireAccount"
          Data.=: disassociateEntireAccount,
        "DataShareArn" Data.=: dataShareArn
      ]
