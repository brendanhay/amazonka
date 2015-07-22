{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Configures an @IndexField@ for the search domain. Used to create new
-- fields and modify existing ones. You must specify the name of the domain
-- you are configuring and an index field configuration. The index field
-- configuration specifies a unique name, the index field type, and the
-- options you want to configure for the field. The options you can specify
-- depend on the @IndexFieldType@. If the field exists, the new
-- configuration replaces the old one. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineIndexField.html>
module Network.AWS.CloudSearch.DefineIndexField
    (
    -- * Request
      DefineIndexField
    -- ** Request constructor
    , defineIndexField
    -- ** Request lenses
    , deffDomainName
    , deffIndexField

    -- * Response
    , DefineIndexFieldResponse
    -- ** Response constructor
    , defineIndexFieldResponse
    -- ** Response lenses
    , defStatus
    , defIndexField
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DefineIndexField@ operation.
-- Specifies the name of the domain you want to update and the index field
-- configuration.
--
-- /See:/ 'defineIndexField' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deffDomainName'
--
-- * 'deffIndexField'
data DefineIndexField = DefineIndexField'
    { _deffDomainName :: !Text
    , _deffIndexField :: !IndexField
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineIndexField' smart constructor.
defineIndexField :: Text -> IndexField -> DefineIndexField
defineIndexField pDomainName pIndexField =
    DefineIndexField'
    { _deffDomainName = pDomainName
    , _deffIndexField = pIndexField
    }

-- | FIXME: Undocumented member.
deffDomainName :: Lens' DefineIndexField Text
deffDomainName = lens _deffDomainName (\ s a -> s{_deffDomainName = a});

-- | The index field and field options you want to configure.
deffIndexField :: Lens' DefineIndexField IndexField
deffIndexField = lens _deffIndexField (\ s a -> s{_deffIndexField = a});

instance AWSRequest DefineIndexField where
        type Sv DefineIndexField = CloudSearch
        type Rs DefineIndexField = DefineIndexFieldResponse
        request = post
        response
          = receiveXMLWrapper "DefineIndexFieldResult"
              (\ s h x ->
                 DefineIndexFieldResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "IndexField"))

instance ToHeaders DefineIndexField where
        toHeaders = const mempty

instance ToPath DefineIndexField where
        toPath = const "/"

instance ToQuery DefineIndexField where
        toQuery DefineIndexField'{..}
          = mconcat
              ["Action" =: ("DefineIndexField" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _deffDomainName,
               "IndexField" =: _deffIndexField]

-- | The result of a @DefineIndexField@ request. Contains the status of the
-- newly-configured index field.
--
-- /See:/ 'defineIndexFieldResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'defStatus'
--
-- * 'defIndexField'
data DefineIndexFieldResponse = DefineIndexFieldResponse'
    { _defStatus     :: !Int
    , _defIndexField :: !IndexFieldStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefineIndexFieldResponse' smart constructor.
defineIndexFieldResponse :: Int -> IndexFieldStatus -> DefineIndexFieldResponse
defineIndexFieldResponse pStatus pIndexField =
    DefineIndexFieldResponse'
    { _defStatus = pStatus
    , _defIndexField = pIndexField
    }

-- | FIXME: Undocumented member.
defStatus :: Lens' DefineIndexFieldResponse Int
defStatus = lens _defStatus (\ s a -> s{_defStatus = a});

-- | FIXME: Undocumented member.
defIndexField :: Lens' DefineIndexFieldResponse IndexFieldStatus
defIndexField = lens _defIndexField (\ s a -> s{_defIndexField = a});
