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
-- Module      : Network.AWS.SES.DeleteReceiptFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP address filter.
--
--
-- For information about managing IP address filters, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DeleteReceiptFilter
    (
    -- * Creating a Request
      deleteReceiptFilter
    , DeleteReceiptFilter
    -- * Request Lenses
    , drfFilterName

    -- * Destructuring the Response
    , deleteReceiptFilterResponse
    , DeleteReceiptFilterResponse
    -- * Response Lenses
    , drfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to delete an IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'deleteReceiptFilter' smart constructor.
newtype DeleteReceiptFilter = DeleteReceiptFilter'
  { _drfFilterName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReceiptFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drfFilterName' - The name of the IP address filter to delete.
deleteReceiptFilter
    :: Text -- ^ 'drfFilterName'
    -> DeleteReceiptFilter
deleteReceiptFilter pFilterName_ =
  DeleteReceiptFilter' {_drfFilterName = pFilterName_}


-- | The name of the IP address filter to delete.
drfFilterName :: Lens' DeleteReceiptFilter Text
drfFilterName = lens _drfFilterName (\ s a -> s{_drfFilterName = a})

instance AWSRequest DeleteReceiptFilter where
        type Rs DeleteReceiptFilter =
             DeleteReceiptFilterResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DeleteReceiptFilterResult"
              (\ s h x ->
                 DeleteReceiptFilterResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteReceiptFilter where

instance NFData DeleteReceiptFilter where

instance ToHeaders DeleteReceiptFilter where
        toHeaders = const mempty

instance ToPath DeleteReceiptFilter where
        toPath = const "/"

instance ToQuery DeleteReceiptFilter where
        toQuery DeleteReceiptFilter'{..}
          = mconcat
              ["Action" =: ("DeleteReceiptFilter" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "FilterName" =: _drfFilterName]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'deleteReceiptFilterResponse' smart constructor.
newtype DeleteReceiptFilterResponse = DeleteReceiptFilterResponse'
  { _drfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReceiptFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drfrsResponseStatus' - -- | The response status code.
deleteReceiptFilterResponse
    :: Int -- ^ 'drfrsResponseStatus'
    -> DeleteReceiptFilterResponse
deleteReceiptFilterResponse pResponseStatus_ =
  DeleteReceiptFilterResponse' {_drfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drfrsResponseStatus :: Lens' DeleteReceiptFilterResponse Int
drfrsResponseStatus = lens _drfrsResponseStatus (\ s a -> s{_drfrsResponseStatus = a})

instance NFData DeleteReceiptFilterResponse where
