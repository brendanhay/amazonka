{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteSuggester
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a suggester. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteSuggester
  ( -- * Creating a request
    DeleteSuggester (..),
    mkDeleteSuggester,

    -- ** Request lenses
    dsgSuggesterName,
    dsgDomainName,

    -- * Destructuring the response
    DeleteSuggesterResponse (..),
    mkDeleteSuggesterResponse,

    -- ** Response lenses
    dsfrsSuggester,
    dsfrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DeleteSuggester' @ operation. Specifies the name of the domain you want to update and name of the suggester you want to delete.
--
-- /See:/ 'mkDeleteSuggester' smart constructor.
data DeleteSuggester = DeleteSuggester'
  { -- | Specifies the name of the suggester you want to delete.
    suggesterName :: Lude.Text,
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSuggester' with the minimum fields required to make a request.
--
-- * 'suggesterName' - Specifies the name of the suggester you want to delete.
-- * 'domainName' -
mkDeleteSuggester ::
  -- | 'suggesterName'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  DeleteSuggester
mkDeleteSuggester pSuggesterName_ pDomainName_ =
  DeleteSuggester'
    { suggesterName = pSuggesterName_,
      domainName = pDomainName_
    }

-- | Specifies the name of the suggester you want to delete.
--
-- /Note:/ Consider using 'suggesterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSuggesterName :: Lens.Lens' DeleteSuggester Lude.Text
dsgSuggesterName = Lens.lens (suggesterName :: DeleteSuggester -> Lude.Text) (\s a -> s {suggesterName = a} :: DeleteSuggester)
{-# DEPRECATED dsgSuggesterName "Use generic-lens or generic-optics with 'suggesterName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDomainName :: Lens.Lens' DeleteSuggester Lude.Text
dsgDomainName = Lens.lens (domainName :: DeleteSuggester -> Lude.Text) (\s a -> s {domainName = a} :: DeleteSuggester)
{-# DEPRECATED dsgDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DeleteSuggester where
  type Rs DeleteSuggester = DeleteSuggesterResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DeleteSuggesterResult"
      ( \s h x ->
          DeleteSuggesterResponse'
            Lude.<$> (x Lude..@ "Suggester") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSuggester where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSuggester where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSuggester where
  toQuery DeleteSuggester' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSuggester" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "SuggesterName" Lude.=: suggesterName,
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DeleteSuggester@ request. Contains the status of the deleted suggester.
--
-- /See:/ 'mkDeleteSuggesterResponse' smart constructor.
data DeleteSuggesterResponse = DeleteSuggesterResponse'
  { -- | The status of the suggester being deleted.
    suggester :: SuggesterStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSuggesterResponse' with the minimum fields required to make a request.
--
-- * 'suggester' - The status of the suggester being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteSuggesterResponse ::
  -- | 'suggester'
  SuggesterStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSuggesterResponse
mkDeleteSuggesterResponse pSuggester_ pResponseStatus_ =
  DeleteSuggesterResponse'
    { suggester = pSuggester_,
      responseStatus = pResponseStatus_
    }

-- | The status of the suggester being deleted.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsSuggester :: Lens.Lens' DeleteSuggesterResponse SuggesterStatus
dsfrsSuggester = Lens.lens (suggester :: DeleteSuggesterResponse -> SuggesterStatus) (\s a -> s {suggester = a} :: DeleteSuggesterResponse)
{-# DEPRECATED dsfrsSuggester "Use generic-lens or generic-optics with 'suggester' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsResponseStatus :: Lens.Lens' DeleteSuggesterResponse Lude.Int
dsfrsResponseStatus = Lens.lens (responseStatus :: DeleteSuggesterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSuggesterResponse)
{-# DEPRECATED dsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
