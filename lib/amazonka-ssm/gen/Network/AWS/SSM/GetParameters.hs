{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details of a parameter. Don't confuse this API action with the 'GetParameter' API action.
module Network.AWS.SSM.GetParameters
  ( -- * Creating a request
    GetParameters (..),
    mkGetParameters,

    -- ** Request lenses
    gpWithDecryption,
    gpNames,

    -- * Destructuring the response
    GetParametersResponse (..),
    mkGetParametersResponse,

    -- ** Response lenses
    gprsParameters,
    gprsInvalidParameters,
    gprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetParameters' smart constructor.
data GetParameters = GetParameters'
  { -- | Return decrypted secure string value. Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
    withDecryption :: Lude.Maybe Lude.Bool,
    -- | Names of the parameters for which you want to query information.
    names :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParameters' with the minimum fields required to make a request.
--
-- * 'withDecryption' - Return decrypted secure string value. Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
-- * 'names' - Names of the parameters for which you want to query information.
mkGetParameters ::
  -- | 'names'
  Lude.NonEmpty Lude.Text ->
  GetParameters
mkGetParameters pNames_ =
  GetParameters' {withDecryption = Lude.Nothing, names = pNames_}

-- | Return decrypted secure string value. Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpWithDecryption :: Lens.Lens' GetParameters (Lude.Maybe Lude.Bool)
gpWithDecryption = Lens.lens (withDecryption :: GetParameters -> Lude.Maybe Lude.Bool) (\s a -> s {withDecryption = a} :: GetParameters)
{-# DEPRECATED gpWithDecryption "Use generic-lens or generic-optics with 'withDecryption' instead." #-}

-- | Names of the parameters for which you want to query information.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpNames :: Lens.Lens' GetParameters (Lude.NonEmpty Lude.Text)
gpNames = Lens.lens (names :: GetParameters -> Lude.NonEmpty Lude.Text) (\s a -> s {names = a} :: GetParameters)
{-# DEPRECATED gpNames "Use generic-lens or generic-optics with 'names' instead." #-}

instance Lude.AWSRequest GetParameters where
  type Rs GetParameters = GetParametersResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetParametersResponse'
            Lude.<$> (x Lude..?> "Parameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "InvalidParameters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetParameters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetParameters where
  toJSON GetParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WithDecryption" Lude..=) Lude.<$> withDecryption,
            Lude.Just ("Names" Lude..= names)
          ]
      )

instance Lude.ToPath GetParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery GetParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetParametersResponse' smart constructor.
data GetParametersResponse = GetParametersResponse'
  { -- | A list of details for a parameter.
    parameters :: Lude.Maybe [Parameter],
    -- | A list of parameters that are not formatted correctly or do not run during an execution.
    invalidParameters :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetParametersResponse' with the minimum fields required to make a request.
--
-- * 'parameters' - A list of details for a parameter.
-- * 'invalidParameters' - A list of parameters that are not formatted correctly or do not run during an execution.
-- * 'responseStatus' - The response status code.
mkGetParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetParametersResponse
mkGetParametersResponse pResponseStatus_ =
  GetParametersResponse'
    { parameters = Lude.Nothing,
      invalidParameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of details for a parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsParameters :: Lens.Lens' GetParametersResponse (Lude.Maybe [Parameter])
gprsParameters = Lens.lens (parameters :: GetParametersResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: GetParametersResponse)
{-# DEPRECATED gprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A list of parameters that are not formatted correctly or do not run during an execution.
--
-- /Note:/ Consider using 'invalidParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsInvalidParameters :: Lens.Lens' GetParametersResponse (Lude.Maybe [Lude.Text])
gprsInvalidParameters = Lens.lens (invalidParameters :: GetParametersResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {invalidParameters = a} :: GetParametersResponse)
{-# DEPRECATED gprsInvalidParameters "Use generic-lens or generic-optics with 'invalidParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetParametersResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetParametersResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
