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
-- Module      : Amazonka.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an AMI to its default value.
module Amazonka.EC2.ResetImageAttribute
  ( -- * Creating a Request
    ResetImageAttribute (..),
    newResetImageAttribute,

    -- * Request Lenses
    resetImageAttribute_dryRun,
    resetImageAttribute_attribute,
    resetImageAttribute_imageId,

    -- * Destructuring the Response
    ResetImageAttributeResponse (..),
    newResetImageAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ResetImageAttribute.
--
-- /See:/ 'newResetImageAttribute' smart constructor.
data ResetImageAttribute = ResetImageAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The attribute to reset (currently you can only reset the launch
    -- permission attribute).
    attribute :: ResetImageAttributeName,
    -- | The ID of the AMI.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'resetImageAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'resetImageAttribute_attribute' - The attribute to reset (currently you can only reset the launch
-- permission attribute).
--
-- 'imageId', 'resetImageAttribute_imageId' - The ID of the AMI.
newResetImageAttribute ::
  -- | 'attribute'
  ResetImageAttributeName ->
  -- | 'imageId'
  Prelude.Text ->
  ResetImageAttribute
newResetImageAttribute pAttribute_ pImageId_ =
  ResetImageAttribute'
    { dryRun = Prelude.Nothing,
      attribute = pAttribute_,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetImageAttribute_dryRun :: Lens.Lens' ResetImageAttribute (Prelude.Maybe Prelude.Bool)
resetImageAttribute_dryRun = Lens.lens (\ResetImageAttribute' {dryRun} -> dryRun) (\s@ResetImageAttribute' {} a -> s {dryRun = a} :: ResetImageAttribute)

-- | The attribute to reset (currently you can only reset the launch
-- permission attribute).
resetImageAttribute_attribute :: Lens.Lens' ResetImageAttribute ResetImageAttributeName
resetImageAttribute_attribute = Lens.lens (\ResetImageAttribute' {attribute} -> attribute) (\s@ResetImageAttribute' {} a -> s {attribute = a} :: ResetImageAttribute)

-- | The ID of the AMI.
resetImageAttribute_imageId :: Lens.Lens' ResetImageAttribute Prelude.Text
resetImageAttribute_imageId = Lens.lens (\ResetImageAttribute' {imageId} -> imageId) (\s@ResetImageAttribute' {} a -> s {imageId = a} :: ResetImageAttribute)

instance Core.AWSRequest ResetImageAttribute where
  type
    AWSResponse ResetImageAttribute =
      ResetImageAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ResetImageAttributeResponse'

instance Prelude.Hashable ResetImageAttribute where
  hashWithSalt _salt ResetImageAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData ResetImageAttribute where
  rnf ResetImageAttribute' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf attribute `Prelude.seq`
        Prelude.rnf imageId

instance Data.ToHeaders ResetImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetImageAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetImageAttribute where
  toQuery ResetImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResetImageAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Attribute" Data.=: attribute,
        "ImageId" Data.=: imageId
      ]

-- | /See:/ 'newResetImageAttributeResponse' smart constructor.
data ResetImageAttributeResponse = ResetImageAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetImageAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResetImageAttributeResponse ::
  ResetImageAttributeResponse
newResetImageAttributeResponse =
  ResetImageAttributeResponse'

instance Prelude.NFData ResetImageAttributeResponse where
  rnf _ = ()
