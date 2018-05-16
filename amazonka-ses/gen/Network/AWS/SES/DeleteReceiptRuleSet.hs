{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteReceiptRuleSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified receipt rule set and all of the receipt rules it contains.
--
--
-- For information about managing receipt rule sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DeleteReceiptRuleSet
    (
    -- * Creating a Request
      deleteReceiptRuleSet
    , DeleteReceiptRuleSet
    -- * Request Lenses
    , dRuleSetName

    -- * Destructuring the Response
    , deleteReceiptRuleSetResponse
    , DeleteReceiptRuleSetResponse
    -- * Response Lenses
    , drrsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to delete a receipt rule set and all of the receipt rules it contains. You use receipt rule sets to receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'deleteReceiptRuleSet' smart constructor.
newtype DeleteReceiptRuleSet = DeleteReceiptRuleSet'
  { _dRuleSetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReceiptRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRuleSetName' - The name of the receipt rule set to delete.
deleteReceiptRuleSet
    :: Text -- ^ 'dRuleSetName'
    -> DeleteReceiptRuleSet
deleteReceiptRuleSet pRuleSetName_ =
  DeleteReceiptRuleSet' {_dRuleSetName = pRuleSetName_}


-- | The name of the receipt rule set to delete.
dRuleSetName :: Lens' DeleteReceiptRuleSet Text
dRuleSetName = lens _dRuleSetName (\ s a -> s{_dRuleSetName = a})

instance AWSRequest DeleteReceiptRuleSet where
        type Rs DeleteReceiptRuleSet =
             DeleteReceiptRuleSetResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DeleteReceiptRuleSetResult"
              (\ s h x ->
                 DeleteReceiptRuleSetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteReceiptRuleSet where

instance NFData DeleteReceiptRuleSet where

instance ToHeaders DeleteReceiptRuleSet where
        toHeaders = const mempty

instance ToPath DeleteReceiptRuleSet where
        toPath = const "/"

instance ToQuery DeleteReceiptRuleSet where
        toQuery DeleteReceiptRuleSet'{..}
          = mconcat
              ["Action" =: ("DeleteReceiptRuleSet" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RuleSetName" =: _dRuleSetName]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'deleteReceiptRuleSetResponse' smart constructor.
newtype DeleteReceiptRuleSetResponse = DeleteReceiptRuleSetResponse'
  { _drrsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReceiptRuleSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsrsResponseStatus' - -- | The response status code.
deleteReceiptRuleSetResponse
    :: Int -- ^ 'drrsrsResponseStatus'
    -> DeleteReceiptRuleSetResponse
deleteReceiptRuleSetResponse pResponseStatus_ =
  DeleteReceiptRuleSetResponse' {_drrsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drrsrsResponseStatus :: Lens' DeleteReceiptRuleSetResponse Int
drrsrsResponseStatus = lens _drrsrsResponseStatus (\ s a -> s{_drrsrsResponseStatus = a})

instance NFData DeleteReceiptRuleSetResponse where
