{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetSDKType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetSDKType
  ( -- * Creating a request
    GetSDKType (..),
    mkGetSDKType,

    -- ** Request lenses
    gstId,

    -- * Destructuring the response
    SDKType (..),
    mkSDKType,

    -- ** Response lenses
    stFriendlyName,
    stConfigurationProperties,
    stId,
    stDescription,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Get an 'SdkType' instance.
--
-- /See:/ 'mkGetSDKType' smart constructor.
newtype GetSDKType = GetSDKType'
  { -- | [Required] The identifier of the queried 'SdkType' instance.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSDKType' with the minimum fields required to make a request.
--
-- * 'id' - [Required] The identifier of the queried 'SdkType' instance.
mkGetSDKType ::
  -- | 'id'
  Lude.Text ->
  GetSDKType
mkGetSDKType pId_ = GetSDKType' {id = pId_}

-- | [Required] The identifier of the queried 'SdkType' instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstId :: Lens.Lens' GetSDKType Lude.Text
gstId = Lens.lens (id :: GetSDKType -> Lude.Text) (\s a -> s {id = a} :: GetSDKType)
{-# DEPRECATED gstId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetSDKType where
  type Rs GetSDKType = SDKType
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetSDKType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetSDKType where
  toPath GetSDKType' {..} = Lude.mconcat ["/sdktypes/", Lude.toBS id]

instance Lude.ToQuery GetSDKType where
  toQuery = Lude.const Lude.mempty
