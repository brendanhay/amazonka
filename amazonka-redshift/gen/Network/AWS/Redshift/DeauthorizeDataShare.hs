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
-- Module      : Network.AWS.Redshift.DeauthorizeDataShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From the producer account, removes authorization from the specified
-- datashare.
module Network.AWS.Redshift.DeauthorizeDataShare
  ( -- * Creating a Request
    DeauthorizeDataShare (..),
    newDeauthorizeDataShare,

    -- * Request Lenses
    deauthorizeDataShare_dataShareArn,
    deauthorizeDataShare_consumerIdentifier,

    -- * Destructuring the Response
    DataShare (..),
    newDataShare,

    -- * Response Lenses
    dataShare_dataShareAssociations,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_producerArn,
    dataShare_dataShareArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeauthorizeDataShare' smart constructor.
data DeauthorizeDataShare = DeauthorizeDataShare'
  { -- | The Amazon Resource Name (ARN) of the datashare to remove authorization
    -- from.
    dataShareArn :: Prelude.Text,
    -- | The identifier of the data consumer that is to have authorization
    -- removed from the datashare. This identifier is an AWS account ID.
    consumerIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeauthorizeDataShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataShareArn', 'deauthorizeDataShare_dataShareArn' - The Amazon Resource Name (ARN) of the datashare to remove authorization
-- from.
--
-- 'consumerIdentifier', 'deauthorizeDataShare_consumerIdentifier' - The identifier of the data consumer that is to have authorization
-- removed from the datashare. This identifier is an AWS account ID.
newDeauthorizeDataShare ::
  -- | 'dataShareArn'
  Prelude.Text ->
  -- | 'consumerIdentifier'
  Prelude.Text ->
  DeauthorizeDataShare
newDeauthorizeDataShare
  pDataShareArn_
  pConsumerIdentifier_ =
    DeauthorizeDataShare'
      { dataShareArn =
          pDataShareArn_,
        consumerIdentifier = pConsumerIdentifier_
      }

-- | The Amazon Resource Name (ARN) of the datashare to remove authorization
-- from.
deauthorizeDataShare_dataShareArn :: Lens.Lens' DeauthorizeDataShare Prelude.Text
deauthorizeDataShare_dataShareArn = Lens.lens (\DeauthorizeDataShare' {dataShareArn} -> dataShareArn) (\s@DeauthorizeDataShare' {} a -> s {dataShareArn = a} :: DeauthorizeDataShare)

-- | The identifier of the data consumer that is to have authorization
-- removed from the datashare. This identifier is an AWS account ID.
deauthorizeDataShare_consumerIdentifier :: Lens.Lens' DeauthorizeDataShare Prelude.Text
deauthorizeDataShare_consumerIdentifier = Lens.lens (\DeauthorizeDataShare' {consumerIdentifier} -> consumerIdentifier) (\s@DeauthorizeDataShare' {} a -> s {consumerIdentifier = a} :: DeauthorizeDataShare)

instance Core.AWSRequest DeauthorizeDataShare where
  type AWSResponse DeauthorizeDataShare = DataShare
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeauthorizeDataShareResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable DeauthorizeDataShare

instance Prelude.NFData DeauthorizeDataShare

instance Core.ToHeaders DeauthorizeDataShare where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeauthorizeDataShare where
  toPath = Prelude.const "/"

instance Core.ToQuery DeauthorizeDataShare where
  toQuery DeauthorizeDataShare' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeauthorizeDataShare" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "DataShareArn" Core.=: dataShareArn,
        "ConsumerIdentifier" Core.=: consumerIdentifier
      ]
