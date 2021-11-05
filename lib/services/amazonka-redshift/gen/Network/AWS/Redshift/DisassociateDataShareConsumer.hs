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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From a consumer account, remove association for the specified datashare.
module Amazonka.Redshift.DisassociateDataShareConsumer
  ( -- * Creating a Request
    DisassociateDataShareConsumer (..),
    newDisassociateDataShareConsumer,

    -- * Request Lenses
    disassociateDataShareConsumer_disassociateEntireAccount,
    disassociateDataShareConsumer_consumerArn,
    disassociateDataShareConsumer_dataShareArn,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateDataShareConsumer' smart constructor.
data DisassociateDataShareConsumer = DisassociateDataShareConsumer'
  { -- | A value that specifies whether association for the datashare is removed
    -- from the entire account.
    disassociateEntireAccount :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the consumer that association for the
    -- datashare is removed from.
    consumerArn :: Prelude.Maybe Prelude.Text,
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
-- 'disassociateEntireAccount', 'disassociateDataShareConsumer_disassociateEntireAccount' - A value that specifies whether association for the datashare is removed
-- from the entire account.
--
-- 'consumerArn', 'disassociateDataShareConsumer_consumerArn' - The Amazon Resource Name (ARN) of the consumer that association for the
-- datashare is removed from.
--
-- 'dataShareArn', 'disassociateDataShareConsumer_dataShareArn' - The Amazon Resource Name (ARN) of the datashare to remove association
-- for.
newDisassociateDataShareConsumer ::
  -- | 'dataShareArn'
  Prelude.Text ->
  DisassociateDataShareConsumer
newDisassociateDataShareConsumer pDataShareArn_ =
  DisassociateDataShareConsumer'
    { disassociateEntireAccount =
        Prelude.Nothing,
      consumerArn = Prelude.Nothing,
      dataShareArn = pDataShareArn_
    }

-- | A value that specifies whether association for the datashare is removed
-- from the entire account.
disassociateDataShareConsumer_disassociateEntireAccount :: Lens.Lens' DisassociateDataShareConsumer (Prelude.Maybe Prelude.Bool)
disassociateDataShareConsumer_disassociateEntireAccount = Lens.lens (\DisassociateDataShareConsumer' {disassociateEntireAccount} -> disassociateEntireAccount) (\s@DisassociateDataShareConsumer' {} a -> s {disassociateEntireAccount = a} :: DisassociateDataShareConsumer)

-- | The Amazon Resource Name (ARN) of the consumer that association for the
-- datashare is removed from.
disassociateDataShareConsumer_consumerArn :: Lens.Lens' DisassociateDataShareConsumer (Prelude.Maybe Prelude.Text)
disassociateDataShareConsumer_consumerArn = Lens.lens (\DisassociateDataShareConsumer' {consumerArn} -> consumerArn) (\s@DisassociateDataShareConsumer' {} a -> s {consumerArn = a} :: DisassociateDataShareConsumer)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DisassociateDataShareConsumerResult"
      (\s h x -> Core.parseXML x)

instance
  Prelude.Hashable
    DisassociateDataShareConsumer

instance Prelude.NFData DisassociateDataShareConsumer

instance Core.ToHeaders DisassociateDataShareConsumer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisassociateDataShareConsumer where
  toPath = Prelude.const "/"

instance Core.ToQuery DisassociateDataShareConsumer where
  toQuery DisassociateDataShareConsumer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DisassociateDataShareConsumer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "DisassociateEntireAccount"
          Core.=: disassociateEntireAccount,
        "ConsumerArn" Core.=: consumerArn,
        "DataShareArn" Core.=: dataShareArn
      ]
