{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Untag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a specified resource group.
module Network.AWS.ResourceGroups.Untag
  ( -- * Creating a request
    Untag (..),
    mkUntag,

    -- ** Request lenses
    uARN,
    uKeys,

    -- * Destructuring the response
    UntagResponse (..),
    mkUntagResponse,

    -- ** Response lenses
    ursARN,
    ursKeys,
    ursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntag' smart constructor.
data Untag = Untag'
  { -- | The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
    arn :: Lude.Text,
    -- | The keys of the tags to be removed.
    keys :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Untag' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
-- * 'keys' - The keys of the tags to be removed.
mkUntag ::
  -- | 'arn'
  Lude.Text ->
  Untag
mkUntag pARN_ = Untag' {arn = pARN_, keys = Lude.mempty}

-- | The ARN of the resource group from which to remove tags. The command removed both the specified keys and any values associated with those keys.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uARN :: Lens.Lens' Untag Lude.Text
uARN = Lens.lens (arn :: Untag -> Lude.Text) (\s a -> s {arn = a} :: Untag)
{-# DEPRECATED uARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The keys of the tags to be removed.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uKeys :: Lens.Lens' Untag [Lude.Text]
uKeys = Lens.lens (keys :: Untag -> [Lude.Text]) (\s a -> s {keys = a} :: Untag)
{-# DEPRECATED uKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

instance Lude.AWSRequest Untag where
  type Rs Untag = UntagResponse
  request = Req.patchJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UntagResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Keys" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Untag where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON Untag where
  toJSON Untag' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Keys" Lude..= keys)])

instance Lude.ToPath Untag where
  toPath Untag' {..} =
    Lude.mconcat ["/resources/", Lude.toBS arn, "/tags"]

instance Lude.ToQuery Untag where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagResponse' smart constructor.
data UntagResponse = UntagResponse'
  { -- | The ARN of the resource group from which tags have been removed.
    arn :: Lude.Maybe Lude.Text,
    -- | The keys of the tags that were removed.
    keys :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the resource group from which tags have been removed.
-- * 'keys' - The keys of the tags that were removed.
-- * 'responseStatus' - The response status code.
mkUntagResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagResponse
mkUntagResponse pResponseStatus_ =
  UntagResponse'
    { arn = Lude.Nothing,
      keys = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the resource group from which tags have been removed.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursARN :: Lens.Lens' UntagResponse (Lude.Maybe Lude.Text)
ursARN = Lens.lens (arn :: UntagResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UntagResponse)
{-# DEPRECATED ursARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The keys of the tags that were removed.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursKeys :: Lens.Lens' UntagResponse (Lude.Maybe [Lude.Text])
ursKeys = Lens.lens (keys :: UntagResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {keys = a} :: UntagResponse)
{-# DEPRECATED ursKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UntagResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UntagResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
