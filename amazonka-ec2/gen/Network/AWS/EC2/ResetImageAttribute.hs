{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an AMI to its default value.
--
-- The productCodes attribute can\'t be reset.
module Network.AWS.EC2.ResetImageAttribute
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest ResetImageAttribute where
  type
    Rs ResetImageAttribute =
      ResetImageAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ResetImageAttributeResponse'

instance Prelude.Hashable ResetImageAttribute

instance Prelude.NFData ResetImageAttribute

instance Prelude.ToHeaders ResetImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ResetImageAttribute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResetImageAttribute where
  toQuery ResetImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ResetImageAttribute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "Attribute" Prelude.=: attribute,
        "ImageId" Prelude.=: imageId
      ]

-- | /See:/ 'newResetImageAttributeResponse' smart constructor.
data ResetImageAttributeResponse = ResetImageAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetImageAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResetImageAttributeResponse ::
  ResetImageAttributeResponse
newResetImageAttributeResponse =
  ResetImageAttributeResponse'

instance Prelude.NFData ResetImageAttributeResponse
