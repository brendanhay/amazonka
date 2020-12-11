{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a parameter by using the parameter name. Don't confuse this API action with the 'GetParameters' API action.
module Network.AWS.SSM.GetParameter
  ( -- * Creating a request
    GetParameter (..),
    mkGetParameter,

    -- ** Request lenses
    gWithDecryption,
    gName,

    -- * Destructuring the response
    GetParameterResponse (..),
    mkGetParameterResponse,

    -- ** Response lenses
    gprsParameter,
    gprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetParameter' smart constructor.
data GetParameter = GetParameter'
  { withDecryption ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParameter' with the minimum fields required to make a request.
--
-- * 'name' - The name of the parameter you want to query.
-- * 'withDecryption' - Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
mkGetParameter ::
  -- | 'name'
  Lude.Text ->
  GetParameter
mkGetParameter pName_ =
  GetParameter' {withDecryption = Lude.Nothing, name = pName_}

-- | Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gWithDecryption :: Lens.Lens' GetParameter (Lude.Maybe Lude.Bool)
gWithDecryption = Lens.lens (withDecryption :: GetParameter -> Lude.Maybe Lude.Bool) (\s a -> s {withDecryption = a} :: GetParameter)
{-# DEPRECATED gWithDecryption "Use generic-lens or generic-optics with 'withDecryption' instead." #-}

-- | The name of the parameter you want to query.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' GetParameter Lude.Text
gName = Lens.lens (name :: GetParameter -> Lude.Text) (\s a -> s {name = a} :: GetParameter)
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetParameter where
  type Rs GetParameter = GetParameterResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetParameterResponse'
            Lude.<$> (x Lude..?> "Parameter") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetParameter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetParameter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetParameter where
  toJSON GetParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WithDecryption" Lude..=) Lude.<$> withDecryption,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath GetParameter where
  toPath = Lude.const "/"

instance Lude.ToQuery GetParameter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetParameterResponse' smart constructor.
data GetParameterResponse = GetParameterResponse'
  { parameter ::
      Lude.Maybe Parameter,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParameterResponse' with the minimum fields required to make a request.
--
-- * 'parameter' - Information about a parameter.
-- * 'responseStatus' - The response status code.
mkGetParameterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetParameterResponse
mkGetParameterResponse pResponseStatus_ =
  GetParameterResponse'
    { parameter = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a parameter.
--
-- /Note:/ Consider using 'parameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsParameter :: Lens.Lens' GetParameterResponse (Lude.Maybe Parameter)
gprsParameter = Lens.lens (parameter :: GetParameterResponse -> Lude.Maybe Parameter) (\s a -> s {parameter = a} :: GetParameterResponse)
{-# DEPRECATED gprsParameter "Use generic-lens or generic-optics with 'parameter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetParameterResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetParameterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetParameterResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
