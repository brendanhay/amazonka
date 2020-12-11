{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the update of one or more parameters of a database in Amazon Lightsail.
--
-- Parameter updates don't cause outages; therefore, their application is not subject to the preferred maintenance window. However, there are two ways in which parameter updates are applied: @dynamic@ or @pending-reboot@ . Parameters marked with a @dynamic@ apply type are applied immediately. Parameters marked with a @pending-reboot@ apply type are applied only after the database is rebooted using the @reboot relational database@ operation.
-- The @update relational database parameters@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
  ( -- * Creating a request
    UpdateRelationalDatabaseParameters (..),
    mkUpdateRelationalDatabaseParameters,

    -- ** Request lenses
    urdpRelationalDatabaseName,
    urdpParameters,

    -- * Destructuring the response
    UpdateRelationalDatabaseParametersResponse (..),
    mkUpdateRelationalDatabaseParametersResponse,

    -- ** Response lenses
    urdprsOperations,
    urdprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRelationalDatabaseParameters' smart constructor.
data UpdateRelationalDatabaseParameters = UpdateRelationalDatabaseParameters'
  { relationalDatabaseName ::
      Lude.Text,
    parameters ::
      [RelationalDatabaseParameter]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRelationalDatabaseParameters' with the minimum fields required to make a request.
--
-- * 'parameters' - The database parameters to update.
-- * 'relationalDatabaseName' - The name of your database for which to update parameters.
mkUpdateRelationalDatabaseParameters ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  UpdateRelationalDatabaseParameters
mkUpdateRelationalDatabaseParameters pRelationalDatabaseName_ =
  UpdateRelationalDatabaseParameters'
    { relationalDatabaseName =
        pRelationalDatabaseName_,
      parameters = Lude.mempty
    }

-- | The name of your database for which to update parameters.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdpRelationalDatabaseName :: Lens.Lens' UpdateRelationalDatabaseParameters Lude.Text
urdpRelationalDatabaseName = Lens.lens (relationalDatabaseName :: UpdateRelationalDatabaseParameters -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: UpdateRelationalDatabaseParameters)
{-# DEPRECATED urdpRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

-- | The database parameters to update.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdpParameters :: Lens.Lens' UpdateRelationalDatabaseParameters [RelationalDatabaseParameter]
urdpParameters = Lens.lens (parameters :: UpdateRelationalDatabaseParameters -> [RelationalDatabaseParameter]) (\s a -> s {parameters = a} :: UpdateRelationalDatabaseParameters)
{-# DEPRECATED urdpParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Lude.AWSRequest UpdateRelationalDatabaseParameters where
  type
    Rs UpdateRelationalDatabaseParameters =
      UpdateRelationalDatabaseParametersResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRelationalDatabaseParametersResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRelationalDatabaseParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.UpdateRelationalDatabaseParameters" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRelationalDatabaseParameters where
  toJSON UpdateRelationalDatabaseParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName),
            Lude.Just ("parameters" Lude..= parameters)
          ]
      )

instance Lude.ToPath UpdateRelationalDatabaseParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRelationalDatabaseParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRelationalDatabaseParametersResponse' smart constructor.
data UpdateRelationalDatabaseParametersResponse = UpdateRelationalDatabaseParametersResponse'
  { operations ::
      Lude.Maybe
        [Operation],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRelationalDatabaseParametersResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkUpdateRelationalDatabaseParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRelationalDatabaseParametersResponse
mkUpdateRelationalDatabaseParametersResponse pResponseStatus_ =
  UpdateRelationalDatabaseParametersResponse'
    { operations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdprsOperations :: Lens.Lens' UpdateRelationalDatabaseParametersResponse (Lude.Maybe [Operation])
urdprsOperations = Lens.lens (operations :: UpdateRelationalDatabaseParametersResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: UpdateRelationalDatabaseParametersResponse)
{-# DEPRECATED urdprsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdprsResponseStatus :: Lens.Lens' UpdateRelationalDatabaseParametersResponse Lude.Int
urdprsResponseStatus = Lens.lens (responseStatus :: UpdateRelationalDatabaseParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRelationalDatabaseParametersResponse)
{-# DEPRECATED urdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
